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
import Assembler.*
import Transformations.*
import ProgramRepresentation.{ifStmt, *}
import CodeBuilders.*
import A1.*



object A4 {
    /** This is an enhanced version of the loadAndRun method from Assignment 1.
     *
     * It loads the give machine language code into memory, writes the specified values to registers 1 and 2,
     * and then runs the CPU on the code that was loaded. The debugger can be invoked by passing the argument
     * debug = true.
     */
    def loadAndRun(code: MachineCode, register1: Word = Word.zero, register2: Word = Word.zero, debug: Boolean = false): State = {
        val initialState =
            setMem(code.words)
                .setReg(1, register1)
                .setReg(2, register2)
        if(debug) Debugger.debug(initialState, code.debugTable)
        else CPU.run(initialState)
    }

    /** A utility method that takes two sequences of words representing `code` and an `array`,
     * loads both into memory, and sets register 1 to the address of the beginning
     * of the array and register 2 to its length in words.
     */
    def loadCodeAndArray(code: Seq[Word], array: Seq[Word]): State = {
        val arrayAddress: Word = Word(encodeUnsigned(code.size * 4))
        val arrayLength: Word = Word(encodeUnsigned(array.size))
        val loadedCode: State = setMem(code)
        val loadedArray: State = setMem(array, loadedCode, arrayAddress)
        loadedArray.setReg(1, arrayAddress).setReg(2, arrayLength)
    }

    /** A utility method that loads code and an array into memory and runs the code.
     * The debugger can be invoked by passing the argument debug = true.
     */
    def loadAndRunArray(code: MachineCode, array: Seq[Word], debug: Boolean = false): State = {
        val state = loadCodeAndArray(code.words, array)
        if(debug) Debugger.debug(state, code.debugTable)
        else CPU.run(state)
    }




    /** Register 1 holds the address of the beginning of an array of 32-bit integers.
     * Register 2 holds the number of elements in the array.
     * If the array is empty place the value -1 in register 3.
     * Else the last element of the array into register 3.
     */

    lazy val lastElement: MachineCode = {
        val code: Code = {
            val arrayBegin = Variable("arrayBegin")
            val arrayOffset = Variable("arrayOffset")
            val lastElementAddress = Variable("lastElementAddress")
            val lastElementValue = Variable("lastElementValue")
            Scope(Seq[Variable](
                arrayBegin, arrayOffset, lastElementAddress, lastElementValue), block(
                write(arrayBegin, Reg(1)),
                write(arrayOffset, Reg(2)),
                assign(
                    lastElementAddress,
                    binOp(varExpr(arrayBegin), plus,
                        binOp(
                            binOp(varExpr(arrayOffset), minus, literal(1)),
                            times,
                            literal(4)
                        )
                    )
                ),
                ifStmt(
                    varExpr(lastElementAddress),
                    geCmp,
                    varExpr(arrayBegin),
                    assign(lastElementValue, deref(varExpr(lastElementAddress))),
                    assign(lastElementValue, literal(-1))
                ),
                varExpr(lastElementValue)
            ))
        }
        compilerA4(code)
    }

    /** Register 1 holds the address of the beginning of an array of 32-bit two's-complement integers.
     * Register 2 holds the number of elements in the array.
     * Determine the maximum of all the elements of the array, write it into register 3.
     * Assume the array is not empty.
     */
    lazy val arrayMaximum: MachineCode = {
        val code: Code = {
            val curMax = Variable("curMax")
            val idx = Variable("idx")
            val arrayStart = Variable("arrayStart")
            val arraySize = Variable("arraySize")
            val curAddress = Variable("curAddress")
            val curVal = Variable("curVal")
            Scope(Seq[Variable](curMax, idx, arrayStart, arraySize, curAddress, curVal), block(
                write(arrayStart, Reg(1)),
                write(arraySize, Reg(2)),
                assign(idx, literal(0)),
                whileLoop(varExpr(idx), ltCmp, varExpr(arraySize), block(
                    assign(curAddress, binOp(varExpr(arrayStart), plus, binOp(
                        varExpr(idx),
                        times,
                        literal(4)
                    ))),
                    assign(curVal, deref(varExpr(curAddress))),
                    ifStmt(varExpr(curVal), gtCmp, varExpr(curMax),
                        assign(curMax, varExpr(curVal))
                    ),
                    assign(idx, binOp(varExpr(idx), plus, literal(1)))
                )),
                varExpr(curMax)
            ))
        }
        compilerA4(code)
    }

    /** Register 1 holds the address of the beginning of an array of 32-bit integers, each representing a character.
     * The integer zero represents a space, and each integer i (1 <= i <= 26) represents the i'th letter of the
     * uppercase alphabet.
     * Register 2 holds the number of elements in the array (can be empty).
     * Your program should output the uppercase characters represented by the integers in the array.
     * The MIPS system allows you to output one character at a time, by storing its ASCII value into the
     * special memory location CPU.printAddr (1111 1111 1111 1111 0000 0000 0000 1100).
     *
     * Hint: use Google to find an ASCII code table.
     */



    lazy val outputLetters: MachineCode = {

        val code: Code = {
            val idx = Variable("idx")
            val arrayStart = Variable("arrayStart")
            val arraySize = Variable("arraySize")
            val curAddress = Variable("curAddress")
            val curVal = Variable("curVal")
            Scope(Seq[Variable](idx, arrayStart, arraySize,
                curAddress, curVal), block(
                write(arrayStart, Reg(1)),
                write(arraySize, Reg(2)),
                assign(idx, literal(0)),
                whileLoop(varExpr(idx), ltCmp, varExpr(arraySize), block(
                    assign(curAddress, binOp(varExpr(arrayStart), plus, binOp(
                        varExpr(idx),
                        times,
                        literal(4)
                    ))),
                    assign(curVal, deref(varExpr(curAddress))),
                    ifStmt(varExpr(curVal), gtCmp, literal(0),
                        print(binOp(varExpr(curVal), plus, literal(64))),
                        print(literal(32))
                    ),
                    assign(idx, binOp(varExpr(idx), plus, literal(1)))
                ))
            ))
        }
        compilerA4(code)
    }

    /** Register 1 holds a 32-bit integer (in two's-complement notation).
     * Your program should format this integer in base 10, print it, then print a newline character.
     */

    lazy val printIntegerCode: Code = {
        val tenPow = Variable("tenPow")
        val num = Variable("num")
        val digit = Variable("digit")
        val quotient = Variable("quotient")
        val hasHitNonZero = Variable("hasHitNonZero")
        Scope(Seq[Variable](tenPow, num, digit, quotient, hasHitNonZero), block(
            write(num, Reg(1)),
            assign(tenPow, literal(1e9.toInt)),
            assign(hasHitNonZero, literal(0)),
            ifStmt(varExpr(num), eqCmp, literal(0),
                print(literal(48)),
            ),
            ifStmt(varExpr(num), ltCmp, literal(0),
                print(literal(45)),
            ),
            Comment("Printing integer"),
            whileLoop(varExpr(tenPow), gtCmp, literal(0), block(
                assign(quotient, binOp(varExpr(num), divide, varExpr(tenPow))),
                assign(digit, binOp(varExpr(quotient), remainder, literal(10))),
                ifStmt(varExpr(digit), ltCmp, literal(0),
                    assign(digit, binOp(varExpr(digit), times, literal(-1))),
                ),
                ifStmt(varExpr(digit), gtCmp, literal(0),
                    assign(hasHitNonZero, literal(1))
                ),
                ifStmt(varExpr(hasHitNonZero), eqCmp, literal(1),
                    print(binOp(literal(48), plus, varExpr(digit)))
                ),
                assign(tenPow, binOp(varExpr(tenPow), divide, literal(10)))
            )),
            Comment("Printing integer completed"),
            print(literal(10))
        ))
    }
    lazy val printInteger: MachineCode = compilerA4(printIntegerCode)
}
