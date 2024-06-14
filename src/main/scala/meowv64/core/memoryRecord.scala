package meowv64.core

import chisel3._

import chisel3.util._


case class memoryRecordConfig(
    memoryCount: Int,
    memorySize: Int,
    memoryBase: Int,
    memoryMask: Int
){

}

class memoryRecord(implicit val config: memoryRecordConfig) extends Module {

  val toCtrl = IO(new Bundle {
    val memRecord = Output(UInt(32.W))
  })
  
  val toExec = IO(new Bundle {
    val memRecord = Input(Bool())
    val memCount = Input(UInt(32.W))  
  })

val mRecord = RegInit(0.U(log2Ceil(config.memorySize).W))
   toCtrl.memRecord  := 0.U
    when(toExec.memRecord){
        mRecord := toExec.memCount + mRecord
    }

    toCtrl.memRecord := mRecord
}

object memoryRecord {

}



