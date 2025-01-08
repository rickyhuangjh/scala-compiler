/*
   Copyright 2024 Ondrej Lhotak. All rights reserved.

   Permission is granted for private study use by students registered in
   CS 241E in the Fall 2024 term.

   The contents of this file may not be published, in whole or in part,
   in print or electronic form.

   The contents of this file may be included in work submitted for CS
   241E assignments in Fall 2024. The contents of this file may not be
   submitted, in whole or in part, for credit in any other course.
*/
package cs241e.assignments

import cs241e.mips.*
import cs241e.Utils.*
import scala.collection.mutable.ArrayBuffer

import scala.annotation.tailrec

/** An assembler that generates machine language words representing MIPS instructions. */

object Assembler {

  /* ## Assignment 1 */

  /* Complete the implementation of the following methods by replacing the `???`. */

  /** Given a sequence of bits, interpret it as an unsigned binary number and return the number.
    *
    * Scala hint: Consult the Scala library documentation for classes such as Seq:
    * https://www.scala-lang.org/api/current/scala/collection/immutable/Seq.html
    *
    * The purpose of this assignment is for *you* to write code that encodes/decodes numbers in binary.
    * Do not submit solutions that just call functions in the Java/Scala standard library to do the
    * conversion for you.
    **/
  def decodeUnsigned(bits: Seq[Boolean]): Long = {
    require(bits.length >= 0)
    require(bits.length <= 32)
    var ans: Long = 0
    for (bit <- bits) ans = ans * 2 + (if (bit) 1 else 0)
    ans
  }

  /** Given a sequence of bits, interpret it as a signed two's-complement binary number and return the number. */
  def decodeSigned(bits: Seq[Boolean]): Long = {
    require(bits.length >= 1)
    require(bits.length <= 32)
    var ans: Long = 0
    val is_neg: Boolean = bits.head
    for (bit <- bits.tail) {
      ans *= 2
      ans += (if (is_neg != bit) 1 else 0)
    }
    if (is_neg) {
      ans += 1
      ans *= -1
    }
    ans
  }

  /** Given a non-negative number `i`, encode it as an unsigned binary number using the number of bits
    * specified by `bits`.
    *
    * Scala hint: The `bits: Int = 32` specifies `32` as the default value of bits. When calling this method, one
    * can specify the number of bits explicitly (e.g. `encodeUnsigned(42, 8)`), or leave it unspecified
    * (e.g. `encodeUnsigned(42)`), in which case the default value of 32 will be used.
    *
    * The length of the output sequence must be equal to `bits`.
    **/
  def encodeUnsigned(i: Long, bits: Int = 32): Seq[Boolean] = {
    require(bits >= 0)
    require(bits <= 32)
    require(i >= 0)
    require(i < twoTo(bits))
    var temp_i: Long = i
    val buffer = ArrayBuffer[Boolean]()
    while (buffer.length < bits) {
      buffer += (temp_i & 1) == 1
      temp_i >>>= 1
    }
    val ans = buffer.reverse
    ans.toArray
  }

  /** Given a number `i`, encode it as a signed two's-complement binary number using the number of bits
    * specified by `bits`.
    *
    * The length of the output sequence must be equal to `bits`.
    **/
  def encodeSigned(i: Long, bits: Int = 32): Seq[Boolean] = {
    require(bits >= 1)
    require(bits <= 32)
    require(i >= -twoTo(bits-1))
    require(i < twoTo(bits-1))
    val buffer = ArrayBuffer[Boolean]()
    var temp_i: Long = i
    while (buffer.length < bits) {
      buffer += (temp_i & 1) == 1
      temp_i >>>= 1
    }
    val ans = buffer.reverse
    ans.toArray
  }


  /* Before continuing Assignment 1, go to `A1.scala` and complete the methods there. Then return here and implement
   * the following.
   */

  

  /* Each of the following methods should encode the corresponding MIPS machine language instruction as a 32-bit `Word`.
   *
   * Hint: One way to create a word is from a sequence of 32 Booleans.
   * One way to create a sequence of Booleans is using Bits.
   *
   * For example:
   * `val fourBits = Seq(true, false, true, false)`
   * `val moreBits = Bits("0101")`
   * `val eightBits = fourBits ++ moreBits`
   * `val word = Word(eightBits ++ eightBits ++ eightBits ++ eightBits)`
   */

  /* Hint: You may implement additional helper methods if you wish to factor out common code. */
  val FIVE_ZEROES = Bits("00000")
  val SIX_ZEROES = Bits("000000")

  def ADD(d: Reg, s: Reg, t: Reg = Reg.zero): Word = {
    val d_bits = encodeUnsigned(d.number, 5)
    val s_bits = encodeUnsigned(s.number, 5)
    val t_bits = encodeUnsigned(t.number, 5)
    val instr_bits = Bits("100000")
    Word(SIX_ZEROES ++ s_bits ++ t_bits ++ d_bits ++ FIVE_ZEROES ++ instr_bits)
  }
  def SUB(d: Reg, s: Reg, t: Reg): Word = {
    val d_bits = encodeUnsigned(d.number, 5)
    val s_bits = encodeUnsigned(s.number, 5)
    val t_bits = encodeUnsigned(t.number, 5)
    val instr_bits = Bits("100010")
    Word(SIX_ZEROES ++ s_bits ++ t_bits ++ d_bits ++ FIVE_ZEROES ++ instr_bits)
  }
  def MULT(s: Reg, t: Reg): Word = {
    val s_bits = encodeUnsigned(s.number, 5)
    val t_bits = encodeUnsigned(t.number, 5)
    val instr_bits = Bits("011000")
    Word(SIX_ZEROES ++ s_bits ++ t_bits ++ FIVE_ZEROES ++ FIVE_ZEROES ++ instr_bits)
  }
  def MULTU(s: Reg, t: Reg): Word = {
    val s_bits = encodeUnsigned(s.number, 5)
    val t_bits = encodeUnsigned(t.number, 5)
    val instr_bits = Bits("011001")
    Word(SIX_ZEROES ++ s_bits ++ t_bits ++ FIVE_ZEROES ++ FIVE_ZEROES ++ instr_bits)
  }
  def DIV(s: Reg, t: Reg): Word = {
    val s_bits = encodeUnsigned(s.number, 5)
    val t_bits = encodeUnsigned(t.number, 5)
    val instr_bits = Bits("011010")
    Word(SIX_ZEROES ++ s_bits ++ t_bits ++ FIVE_ZEROES ++ FIVE_ZEROES ++ instr_bits)
  }
  def DIVU(s: Reg, t: Reg): Word = {
    val s_bits = encodeUnsigned(s.number, 5)
    val t_bits = encodeUnsigned(t.number, 5)
    val instr_bits = Bits("011011")
    Word(SIX_ZEROES ++ s_bits ++ t_bits ++ FIVE_ZEROES ++ FIVE_ZEROES ++ instr_bits)
  }
  def MFHI(d: Reg): Word = {
    val d_bits = encodeUnsigned(d.number, 5)
    val instr_bits = Bits("010000")
    Word(SIX_ZEROES ++ FIVE_ZEROES ++ FIVE_ZEROES ++ d_bits ++ FIVE_ZEROES ++ instr_bits)
  }
  def MFLO(d: Reg): Word = {
    val d_bits = encodeUnsigned(d.number, 5)
    val instr_bits = Bits("010010")
    Word(SIX_ZEROES ++ FIVE_ZEROES ++ FIVE_ZEROES ++ d_bits ++ FIVE_ZEROES ++ instr_bits)
  }
  def LIS(d: Reg): Word = {
    val d_bits = encodeUnsigned(d.number, 5)
    val instr_bits = Bits("010100")
    Word(SIX_ZEROES ++ FIVE_ZEROES ++ FIVE_ZEROES ++ d_bits ++ FIVE_ZEROES ++ instr_bits)
  }
  def LW(t: Reg, i: Int, s: Reg): Word = {
    val s_bits = encodeUnsigned(s.number, 5)
    val t_bits = encodeUnsigned(t.number, 5)
    val instr_bits = Bits("100011")
    val imm_bits = encodeSigned(i, 16)
    Word(instr_bits ++ s_bits ++ t_bits ++ imm_bits)
  }
  def SW(t: Reg, i: Int, s: Reg): Word = {
    val s_bits = encodeUnsigned(s.number, 5)
    val t_bits = encodeUnsigned(t.number, 5)
    val instr_bits = Bits("101011")
    val imm_bits = encodeSigned(i, 16)
    Word(instr_bits ++ s_bits ++ t_bits ++ imm_bits)
  }
  def SLT(d: Reg, s: Reg, t: Reg): Word = {
    val s_bits = encodeUnsigned(s.number, 5)
    val t_bits = encodeUnsigned(t.number, 5)
    val d_bits = encodeUnsigned(d.number, 5)
    val instr_bits = Bits("101010")
    Word(SIX_ZEROES ++ s_bits ++ t_bits ++ d_bits ++ FIVE_ZEROES ++ instr_bits)
  }
  def SLTU(d: Reg, s: Reg, t: Reg): Word = {
    val s_bits = encodeUnsigned(s.number, 5)
    val t_bits = encodeUnsigned(t.number, 5)
    val d_bits = encodeUnsigned(d.number, 5)
    val instr_bits = Bits("101011")
    Word(SIX_ZEROES ++ s_bits ++ t_bits ++ d_bits ++ FIVE_ZEROES ++ instr_bits)
  }
  def BEQ(s: Reg, t: Reg, i: Int): Word = {
    val s_bits = encodeUnsigned(s.number, 5)
    val t_bits = encodeUnsigned(t.number, 5)
    val instr_bits = Bits("000100")
    val imm_bits = encodeSigned(i, 16)
    Word(instr_bits ++ s_bits ++ t_bits ++ imm_bits)
  }
  def BNE(s: Reg, t: Reg, i: Int): Word = {
    val s_bits = encodeUnsigned(s.number, 5)
    val t_bits = encodeUnsigned(t.number, 5)
    val instr_bits = Bits("000101")
    val imm_bits = encodeSigned(i, 16)
    Word(instr_bits ++ s_bits ++ t_bits ++ imm_bits)
  }
  def JR(s: Reg): Word = {
    val s_bits = encodeUnsigned(s.number, 5)
    val instr_bits = Bits("001000")
    Word(SIX_ZEROES ++ s_bits ++ FIVE_ZEROES ++ FIVE_ZEROES ++ FIVE_ZEROES ++ instr_bits)
  }
  def JALR(s: Reg): Word = {
    val s_bits = encodeUnsigned(s.number, 5)
    val instr_bits = Bits("001001")
    Word(SIX_ZEROES ++ s_bits ++ FIVE_ZEROES ++ FIVE_ZEROES ++ FIVE_ZEROES ++ instr_bits)
  }
}
