package KXCore.common.utils

import chisel3._
import chisel3.util._

class VivadoMultiplierIP(dwidth: Int, pipeDepth: Int) extends BlackBox {
    val io = IO(new Bundle{
        val CLK = Input(Clock())
        val A = Input(UInt(dwidth.W))
        val B = Input(UInt(dwidth.W))
        val P = Output(UInt((dwidth * 2).W))
        val CE = Input(Bool())
        val SCLR = Input(Bool()) 
    })
}

class VivadoDividerIP(dwidth: Int) extends BlackBox {
    val io = IO(new Bundle{
        val aclk                      = Input(Clock())
        val aclken                    = Input(Bool())
        val aresetn                   = Input(Bool())
        val s_axis_divisor_tvalid     = Input(Bool())
        val s_axis_divisor_tdata      = Input(UInt(dwidth.W))
        val s_axis_dividend_tvalid    = Input(Bool())
        val s_axis_dividend_tdata     = Input(UInt(dwidth.W))
        val m_axis_dout_tvalid        = Output(Bool())
        val m_axis_dout_tuser         = Output(UInt(1.W))
        val m_axis_dout_tdata         = Output(UInt((dwidth * 2).W))
    })
}