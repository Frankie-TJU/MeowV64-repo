package meowv64.util
import chisel3._
import chisel3.util._

class MultiQueueIO[T <: Data](private val gen: T, val THROUGHPUT: Int)
    extends Bundle {
  val view = Input(Vec(THROUGHPUT, gen))
  val cnt = Input(UInt(log2Ceil(THROUGHPUT + 1).W))
  val accept = Output(UInt(log2Ceil(THROUGHPUT + 1).W))
}

class MultiQueue[T <: Data](
    private val gen: T,
    val DEPTH: Int,
    val INPUT: Int,
    val OUTPUT: Int
) extends Module {
  val reader = IO(Flipped(new MultiQueueIO(gen, OUTPUT)))
  val writer = IO(new MultiQueueIO(gen, INPUT))
  val flush = IO(Input(Bool()))

  // val CNT = Integer.max(coredef.FETCH_NUM, coredef.ISSUE_NUM)
  // val SIZE = (coredef.ISSUE_FIFO_DEPTH.toDouble / CNT).ceil.toInt
  val CNT = Integer.max(INPUT, OUTPUT)
  val count = IO(Output(UInt(log2Ceil(CNT * DEPTH + 1).W)))

  val queues = (0 until CNT).map(idx => {
    val mod = Module(new FlushableQueue(gen, DEPTH))
    mod.suggestName(s"queue_$idx")
    mod
  })
  count := queues.map(queue => queue.io.count).reduce(_ + _)

  for (q <- queues) {
    q.io.flush.get := flush
  }

  val readies = PopCount(queues.map(_.io.enq.ready))
  val valids = PopCount(queues.map(_.io.deq.valid))
  reader.cnt := valids.min(OUTPUT.U)
  writer.accept := readies.min(INPUT.U)

  for (queue <- queues) {
    queue.io.enq.valid := false.B
    queue.io.enq.bits := DontCare
    queue.io.deq.ready := false.B
  }

  val wptr = RegInit(0.U(log2Ceil(CNT).W))
  val rptr = RegInit(0.U(log2Ceil(CNT).W))
  assert (isPow2(CNT))

  for (i <- (0 until INPUT)) {
    for (j <- (0 until CNT)) {
      when(wptr + i.U === j.U) {
        when(writer.cnt > i.U) {
          queues(j).io.enq.bits := writer.view(i)
          queues(j).io.enq.valid := true.B
          assert(queues(j).io.enq.fire)
        }
      }
    }
  }

  for (i <- (0 until OUTPUT)) {
    // set to zero when invalid
    reader.view(i) := 0.U.asTypeOf(gen)

    for (j <- (0 until CNT)) {
      when(rptr + i.U === j.U) {
        when(queues(j).io.deq.valid) {
          reader.view(i) := queues(j).io.deq.bits
        }

        when(reader.accept > i.U) {
          queues(j).io.deq.ready := true.B
          assert(queues(j).io.deq.fire)
        }
      }
    }
  }

  rptr := rptr +% reader.accept
  wptr := wptr +% writer.cnt

  when(flush) {
    wptr := 0.U
    rptr := 0.U
  }
}
