package meowv64.exec

import chisel3._
import chisel3.util.Mux1H
import chisel3.util.PopCount
import chisel3.util.Valid
import chisel3.util.log2Ceil
import meowv64.core.CSRWriter
import meowv64.core.CoreDef
import meowv64.core.PortInfo
import meowv64.core.PrivLevel
import meowv64.core.RegInfo
import meowv64.core.Status
import meowv64.core.VState
import meowv64.instr.Instr
import meowv64.reg.RegType

import scala.collection.mutable

trait UnitSelIO {
  val flush: Bool

  /** Instruction issued from port
    */
  val issue: RegisterReadEgress

  /** Retire port, one for each possible register type
    */
  val retire: RetirementIO
  val extras: mutable.HashMap[String, Data]
}

/** Read instructions from issue queues, and send them into (probably one of
  * multiple) exec unit
  *
  * We use a MPSC FIFO to consume outputs of individual execution units.
  */
class UnitSel(
    portInfo: PortInfo,
    gen: => Seq[ExecUnitInt],
    arbitration: Instr => Seq[Bool],
    bypassIdx: Option[Int] = None,
    /** Add additional pipeline register to improve timing
      */
    hasPipe: Boolean = true
)(implicit val coredef: CoreDef)
    extends Module
    with UnitSelIO {
  val units = gen

  val flush = IO(Input(Bool()))

  val regInfo = portInfo.regInfo
  println(s"Input Register Type: ${regInfo.regType}")

  /** Input from register read stage
    */
  val issue = IO(Flipped(new RegisterReadEgress(regInfo)))

  /** Retired instruction
    */
  val retire = IO(new RetirementIO(portInfo))

  // Extra ports
  val extras = new mutable.HashMap[String, Data]()
  for (u <- units) {
    if (u.isInstanceOf[WithCSRWriter]) {
      println("Found extra port: CSR")
      val csr = IO(new CSRWriter())
      u.asInstanceOf[WithCSRWriter].writer <> csr
      extras.put("CSR", csr)
    }

    if (u.isInstanceOf[WithPrivPort]) {
      println("Found extra port: priv")

      val priv = extras.get("priv") match {
        case Some(port) => port
        case None => {
          val priv = IO(Input(PrivLevel()))
          extras.put("priv", priv)
          priv
        }
      }

      u.asInstanceOf[WithPrivPort].priv := priv
    }

    if (u.isInstanceOf[WithStatus]) {
      println("Found extra port: status")

      val status = extras.get("status") match {
        case Some(port) => port
        case None => {
          val status = IO(Input(new Status))
          extras.put("status", status)
          status
        }
      }

      u.asInstanceOf[WithStatus].status := status
    }

    if (u.isInstanceOf[WithFloatToMem]) {
      println("Found extra port: floatToMem")

      val floatToMem = extras.get("floatToMem") match {
        case Some(port) => port
        case None => {
          val req = IO(Valid(new FloatToMemReq))
          extras.put("floatToMem", req)
          req
        }
      }

      u.asInstanceOf[WithFloatToMem].toMem <> floatToMem
    }

    if (u.isInstanceOf[WithVectorToMem]) {
      println("Found extra port: vectorToMem")

      val floatToMem = extras.get("vectorToMem") match {
        case Some(port) => port
        case None => {
          val req = IO(Valid(new VectorToMemReq))
          extras.put("vectorToMem", req)
          req
        }
      }

      u.asInstanceOf[WithVectorToMem].toMem <> floatToMem
    }

    if (u.isInstanceOf[WithVState]) {
      println("Found extra port: vState")

      val vState = extras.get("vState") match {
        case Some(port) => port
        case None => {
          val vState = IO(Input(new VState))
          extras.put("vState", vState)
          vState
        }
      }

      u.asInstanceOf[WithVState].vState := vState
    }

    if (u.isInstanceOf[WithFRM]) {
      println("Found extra port: frm")

      val frm = extras.get("frm") match {
        case Some(port) => port
        case None => {
          val frm = IO(Input(UInt(3.W)))
          extras.put("frm", frm)
          frm
        }
      }

      u.asInstanceOf[WithFRM].frm := frm
    }
  }

  // pass flush to units
  for (u <- units) {
    u.io.flush := flush
  }

  // Arbitration
  for (u <- units) {
    u.io.next := issue.instr.bits
    u.io.next.instr.valid := false.B
  }

  val execMap = Wire(Vec(units.length, Bool()))
  chisel3.dontTouch(execMap)
  execMap := arbitration(issue.instr.bits.instr.instr)

  // Contains a bypass unit, bypassing all invalid instructions to there
  // e.g. page fault, decoded exec unit might be inaccurate
  if (bypassIdx.isDefined) {
    when(issue.instr.bits.illegal) {
      execMap := VecInit(Seq.fill(units.length)(false.B))
      execMap(bypassIdx.get) := true.B
    }
  }

  // Asserts exactly one can exec this instr
  val execMapUInt = execMap.asUInt
  val execMapCheckOneHot = PopCount(execMapUInt) <= 1.U
  assert(!issue.instr.valid || execMapCheckOneHot)

  val pipeExecMap = RegInit(VecInit(Seq.fill(units.length)(false.B)))
  val pipeInstr = RegInit(IssueQueueInstr.empty())
  val pipeInstrValid = RegInit(false.B)

  val pipeInput = Wire(Bool())

  issue.instr.ready := false.B

  // add additional pipe register
  if (hasPipe) {
    when(pipeInstrValid) {
      pipeInput := false.B
      for ((u, e) <- units.zip(pipeExecMap)) {
        when(e) {
          u.io.next := true.B
          pipeInput := !u.io.stall
        }
      }
    }.otherwise {
      pipeInput := true.B
    }

    when(pipeInput) {
      pipeInstr := issue.instr.bits
      pipeExecMap := execMap
      pipeInstrValid := issue.instr.valid
      issue.instr.ready := issue.instr.valid
    }
  } else {
    pipeInput := DontCare
    // connect to units directly
    when(issue.instr.valid) {
      issue.instr.ready := !Mux1H(execMap.zip(units.map(_.io.stall)))
      for ((u, e) <- units.zip(execMap)) {
        when(e) {
          u.io.next.valid := true.B
        }
      }
    }
  }

  // Retire FIFO and friends

  // Puts into retire FIFO

  val maxDepth = units.map(_.DEPTH).max
  val noDelayUnitCount = units.count(_.DEPTH == 0)
  val fifoDepth =
    units.map(_.DEPTH).sum - maxDepth + units.size - noDelayUnitCount + 2

  // One extra cell for asserting retireTail never reaches retireHead

  if (units.length == 1) {
    println("UnitSel: Single unit")
    val pipeRetire = RegInit(Retirement.empty(regInfo))
    val pipeValid = RegInit(false.B)
    retire.bits := pipeRetire
    retire.valid := pipeValid
    when(units(0).io.stall) {
      pipeRetire := Retirement.empty(regInfo)
      pipeValid := false.B
    }.otherwise {
      pipeRetire := Retirement.from(units(0).io)
      pipeValid := units(0).io.retiredInstr.valid
    }
    when(flush) {
      pipeRetire := Retirement.empty(regInfo)
      pipeValid := false.B
    }
  } else if (maxDepth == 0) {
    println("UnitSel: All units have 0 delay")
    val pipeRetire = RegInit(Retirement.empty(regInfo))
    val pipeValid = RegInit(false.B)
    retire.bits := pipeRetire
    retire.valid := pipeValid
    val validMap = units.map(u => !u.io.stall && u.io.retiredInstr.instr.valid)
    pipeRetire := Mux1H(validMap.zip(units.map(u => Retirement.from(u.io))))
    pipeValid := Mux1H(validMap.zip(units.map(u => u.io.retiredInstr.valid)))
    when(!VecInit(validMap).asUInt.orR || flush) {
      pipeRetire := Retirement.empty(regInfo)
      pipeValid := false.B
    }
  } else {
    println(s"UnitSel: with FIFO depth $fifoDepth")

    val retireFifo = RegInit(
      VecInit(Seq.fill(fifoDepth)(Retirement.empty(regInfo)))
    )
    val retireHead = RegInit(0.U(log2Ceil(fifoDepth).W))
    val retireTail = RegInit(0.U(log2Ceil(fifoDepth).W))

    // MPSC FIFO
    var prevTail = retireTail
    for (u <- units) {
      val newTail = Wire(UInt(log2Ceil(fifoDepth).W))
      newTail := prevTail
      when(!u.io.stall && u.io.retiredInstr.valid) {
        retireFifo(prevTail) := Retirement.from(u.io)
        newTail := Mux(prevTail === (fifoDepth - 1).U, 0.U, prevTail +% 1.U)
      }

      assert(prevTail === retireHead || newTail =/= retireHead)
      prevTail = newTail
    }
    retireTail := prevTail

    // Output

    when(retireTail === retireHead) {
      retire.bits := Retirement.empty(regInfo)
      retire.valid := false.B
    }.otherwise {
      retire.bits := retireFifo(retireHead)
      retire.valid := true.B
      when(retire.fire) {
        retireHead := Mux(
          retireHead === (fifoDepth - 1).U,
          0.U,
          retireHead +% 1.U
        )
      }
    }

    when(flush) {
      retireHead := 0.U
      retireTail := 0.U
    }
  }

  // Flush
  when(flush) {
    pipeInstrValid := false.B
  }
}

class Retirement(val regInfo: RegInfo)(implicit
    val coredef: CoreDef
) extends Bundle {

  /** Instruction info
    */
  val writeRdEff = Bool()
  val rdPhys = UInt(log2Ceil(coredef.MAX_PHYSICAL_REGISTERS).W)
  val rdType = RegType()
  val robIndex = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)

  val info = new RetireInfo(regInfo)
}

class RetirementIO(val portInfo: PortInfo)(implicit
    val coredef: CoreDef
) extends Bundle {
  val bits = Output(
    new Retirement(coredef.REG_MAPPING(portInfo.widestWriteRegType))
  )

  val valid = Output(Bool())
  val ready = Input(Vec(portInfo.writeRegTypes.length, Bool()))
  def fire = {
    val effReady = WireInit(false.B)
    for ((regType, i) <- portInfo.writeRegTypes.zipWithIndex) {
      when(bits.rdType === regType) {
        effReady := ready(i)
      }
    }
    valid && effReady
  }
}

object Retirement {
  def empty(regInfo: RegInfo)(implicit
      coredef: CoreDef
  ): Retirement = {
    val ret = Wire(new Retirement(regInfo))
    ret.writeRdEff := false.B
    ret.rdPhys := 0.U
    ret.rdType := RegType.integer
    ret.robIndex := 0.U
    ret.info := RetireInfo.vacant(regInfo)

    ret
  }

  def from(port: ExecUnitPort)(implicit coredef: CoreDef): Retirement = {
    val ret = Wire(new Retirement(port.regInfo))
    ret.writeRdEff := port.retiredInstr.instr.instr.writeRdEff
    ret.rdType := port.retiredInstr.instr.instr.getRdType()
    ret.rdPhys := port.retiredInstr.rdPhys
    ret.robIndex := port.retiredInstr.robIndex
    ret.info := port.retirement

    ret
  }
}
