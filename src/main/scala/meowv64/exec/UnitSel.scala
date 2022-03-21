package meowv64.exec

import chisel3._
import chisel3.util.Mux1H
import chisel3.util.PopCount
import chisel3.util.log2Ceil
import meowv64.core.CSRWriter
import meowv64.core.CoreDef
import meowv64.core.PrivLevel
import meowv64.core.RegInfo
import meowv64.core.Status
import meowv64.instr.Instr
import meowv64.reg.RegType

import scala.collection.mutable

trait UnitSelIO {
  val flush: Bool
  val rs: RegisterReadEgress
  val retire: Retirement
  val extras: mutable.HashMap[String, Data]
}

/** Read instructions from issue queues, and send them into (probably one of
  * multiple) exec unit
  *
  * We use a MPSC FIFO to consume outputs of individual execution units.
  */
class UnitSel(
    regInfo: RegInfo,
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

  println(s"Register Type: ${regInfo.regType}")

  /** Input from register read stage
    */
  val rs = IO(Flipped(new RegisterReadEgress(regInfo)))

  /** Retired instruction
    */
  val retire = IO(Output(new Retirement(regInfo)))

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
  }

  // pass flush to units
  for (u <- units) {
    u.io.flush := flush
  }

  // Arbitration
  for (u <- units) {
    u.io.next := PipeInstr.empty(regInfo)
  }

  val execMap = Wire(Vec(units.length, Bool()))
  chisel3.dontTouch(execMap)
  execMap := arbitration(rs.instr.bits.instr.instr)

  // Contains a bypass unit, bypassing all invalid instructions to there
  // e.g. page fault, decoded exec unit might be inaccurate
  if (bypassIdx.isDefined) {
    when(rs.instr.bits.illegal) {
      execMap := VecInit(Seq.fill(units.length)(false.B))
      execMap(bypassIdx.get) := true.B
    }
  }

  // Asserts exactly one can exec this instr
  val execMapUInt = execMap.asUInt
  val execMapCheckOneHot = PopCount(execMapUInt) <= 1.U
  assert(!rs.instr.valid || execMapCheckOneHot)

  val pipeExecMap = RegInit(VecInit(Seq.fill(units.length)(false.B)))
  val pipeInstr = RegInit(IssueQueueInstr.empty())
  val pipeInstrValid = RegInit(false.B)

  val pipeInput = Wire(Bool())

  rs.instr.ready := false.B

  if (hasPipe) {
    when(pipeInstrValid) {
      pipeInput := false.B
      for ((u, e) <- units.zip(pipeExecMap)) {
        when(e) {
          u.io.next := pipeInstr
          pipeInput := !u.io.stall
        }
      }
    }.otherwise {
      pipeInput := true.B
    }

    when(pipeInput) {
      pipeInstr := rs.instr.bits
      pipeExecMap := execMap
      pipeInstrValid := rs.instr.valid
      rs.instr.ready := rs.instr.valid
    }
  } else {
    pipeInput := DontCare
    when(rs.instr.valid) {
      rs.instr.ready := !Mux1H(execMap.zip(units.map(_.io.stall)))
      for ((u, e) <- units.zip(execMap)) {
        when(e) {
          u.io.next := rs.instr.bits
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
    retire := pipeRetire
    when(units(0).io.stall) {
      pipeRetire := Retirement.empty(regInfo)
    }.otherwise {
      pipeRetire := Retirement.from(units(0).io)
    }
    when(flush) {
      pipeRetire := Retirement.empty(regInfo)
    }
  } else if (maxDepth == 0) {
    println("UnitSel: All units have 0 delay")
    val pipeRetire = RegInit(Retirement.empty(regInfo))
    retire := pipeRetire
    val validMap = units.map(u => !u.io.stall && u.io.retired.instr.valid)
    pipeRetire := Mux1H(validMap.zip(units.map(u => Retirement.from(u.io))))
    when(!VecInit(validMap).asUInt.orR || flush) {
      pipeRetire := Retirement.empty(regInfo)
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
      when(!u.io.stall && u.io.retired.instr.valid) {
        retireFifo(prevTail) := Retirement.from(u.io)
        newTail := Mux(prevTail === (fifoDepth - 1).U, 0.U, prevTail +% 1.U)
      }

      assert(prevTail === retireHead || newTail =/= retireHead)
      prevTail = newTail
    }
    retireTail := prevTail

    // Output

    when(retireTail === retireHead) {
      retire := Retirement.empty(regInfo)
    }.otherwise {
      retire := retireFifo(retireHead)
      retireHead := Mux(
        retireHead === (fifoDepth - 1).U,
        0.U,
        retireHead +% 1.U
      )
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

  /** This retirement is valid
    */
  val valid = Bool()

  /** Instruction info
    */
  val writeRdEff = Bool()
  val rdPhys = UInt(log2Ceil(regInfo.physRegs).W)
  val rdType = RegType()
  val robIndex = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)

  val info = new RetireInfo(regInfo)
}

object Retirement {
  def empty(regInfo: RegInfo)(implicit
      coredef: CoreDef
  ): Retirement = {
    val ret = Wire(new Retirement(regInfo))
    ret.valid := false.B
    ret.writeRdEff := false.B
    ret.rdPhys := 0.U
    ret.rdType := RegType.integer
    ret.robIndex := 0.U
    ret.info := RetireInfo.vacant(regInfo)

    ret
  }

  def from(port: ExecUnitPort)(implicit coredef: CoreDef): Retirement = {
    val ret = Wire(new Retirement(port.regInfo))
    ret.valid := port.retired.instr.valid
    ret.writeRdEff := port.retired.instr.instr.writeRdEff
    ret.rdType := port.retired.instr.instr.getRdType()
    ret.rdPhys := port.retired.rdPhys
    ret.robIndex := port.retired.robIndex
    ret.info := port.retirement

    ret
  }
}
