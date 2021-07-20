package memTest

import chisel3._
import chisel3.util._

import craft.ShiftRegisterMem
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

class ShiftRegisterIO[T <: Data](gen: T) extends Bundle {
  val in = Input(gen.cloneType)
  val out1 = Output(gen.cloneType)
  val out2 = Output(gen.cloneType)

  val en1 = Input(Bool())
  val en2 = Input(Bool())

  val valid_out1 = Output(Bool())
  val valid_out2 = Output(Bool())

  override def cloneType: this.type = (new ShiftRegisterIO(gen)).asInstanceOf[this.type]
}

class ShiftRegisterMemExample[T <: Data](gen: T, n1: Int, n2: Int) extends Module {
  require (n1 >= 0, "Shift register must have non-negative shift")
  require (n2 >= 0, "Shift register must have non-negative shift")

  val io = IO(new ShiftRegisterIO(gen))
  val logn1 = log2Ceil(n1)
  val logn2 = log2Ceil(n2)
  val cnt1 = RegInit(0.U(logn1.W))
  val cnt2 = RegInit(0.U(logn2.W))

  // add here parameter sram = false, should forward this parameter to SyncReadMem inside ShiftRegisterMem so that annotation knows that black box for sram should not be generated
  val shiftMem1 = ShiftRegisterMem(io.in, n1, io.en1, name = "simple_shift_register1")
  // add here parameter sram = true, should forward this parameter to SyncReadMem inside ShiftRegisterMem so that annotation knows that black box for sram should be generated
  val shiftMem2 = ShiftRegisterMem(io.in, n2, io.en2, name = "simple_shift_register2")

  io.out1 := shiftMem1
  io.out2 := shiftMem2

  when (io.en1 === true.B) {
    cnt1 := cnt1 +% 1.U
  }
  when (io.en2 === true.B) {
    cnt2 := cnt2 +% 1.U
  }

  val initialInDone1 = RegInit(false.B)
  when (cnt1 === (n1.U - 1.U)) {
    initialInDone1 := true.B
  }
  val initialInDone2 = RegInit(false.B)
  when (cnt2 === (n2.U - 1.U)) {
    initialInDone2 := true.B
  }
  io.valid_out1 := initialInDone1 && io.en1
  io.valid_out2 := initialInDone2 && io.en2
}

// By running this app, repl-seq-mem annotates both SyncReadMem memories instantiated inside ShiftRegisterMem but our intention is to map only the larger one.
object ShiftRegisterMemDifferentSizeApp extends App
{
  val n1 = 128
  val n2 = 256
  val dataType = UInt(16.W)

  val arguments = Array(
    "-X", "verilog",
    "--repl-seq-mem", "-c:ShiftRegisterMemExample:-o:mem.conf",
    "--log-level", "info"
  )
  // generate black boxes for memories
  (new ChiselStage).execute(arguments, Seq(ChiselGeneratorAnnotation(() => new ShiftRegisterMemExample(dataType, n1, n2))))
}

// By running this app, repl-seq-mem annotates both SyncReadMem memories instantiated inside ShiftRegisterMem but our intention is to map only second, even though they have the same size.
object ShiftRegisterMemSameSizeApp extends App
{
  val n1 = 128
  val n2 = 128
  val dataType = UInt(32.W)

  val arguments = Array(
    "-X", "verilog",
    "--repl-seq-mem", "-c:ShiftRegisterMemExample:-o:mem.conf",
    "--log-level", "info"
  )
  // generate black boxes for memories
  (new ChiselStage).execute(arguments, Seq(ChiselGeneratorAnnotation(() => new ShiftRegisterMemExample(dataType, n1, n2))))
}
