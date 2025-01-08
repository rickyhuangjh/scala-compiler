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

import ProgramRepresentation.*
import Assembler.*
import cs241e.mips.*

/** Methods that generate `Code` for various higher-level language features. */

object CodeBuilders {
    /* ## Assignment 4 */

    /* Complete the implementation of the following methods by replacing the `???`. */

    /** Generates a binary operation that evaluates `e1` and `e2`, ensures that the `Reg.result` from
     * `e1` is in `Reg.scratch` and that the `Reg.result` from `e2` is still in `Reg.result`,
     * then executes `op`.
     *
     * Hint: Use a temporary variable and a `Scope`.
     *
     * The generated code may modify the values of Reg.result and Reg.scratch. If you
     * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
     * must not modify the values of any other registers that are already listed in Reg.scala.
     */
    def binOp(e1: Code, op: Code, e2: Code): Code = {
        val binOpTemp = Variable("binOpTemp")
        Scope(Seq[Variable](binOpTemp), block(
            e1,
            VarAccess(Reg.result, binOpTemp, false),
            e2,
            VarAccess(Reg.scratch, binOpTemp, true),
            op
        ))
    }

    /* The following `Code`s are intended to be used as the `op` argument to `binOp`.
     * They should expect two operands in `Reg.scratch` and `Reg.result`, compute the corresponding arithmetic
     * operation, and leave the result of the operation in `Reg.result`.
     *
     * Assume that the operands are to be interpreted as signed two's-complement integers except in
     * `divideUnsigned` and `remainderUnsigned`.
     *
     * The generated code may modify the values of Reg.result and Reg.scratch. If you
     * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
     * must not modify the values of any other registers that are already listed in Reg.scala.
     */
    lazy val plus: Code = ADD(Reg.result, Reg.scratch, Reg.result)
    lazy val minus: Code = SUB(Reg.result, Reg.scratch, Reg.result)
    lazy val times: Code = block(
        MULT(Reg.scratch, Reg.result),
        MFLO(Reg.result)
    )
    lazy val divide: Code = block(
        DIV(Reg.scratch, Reg.result),
        MFLO(Reg.result)
    )
    lazy val remainder: Code = block(
        DIV(Reg.scratch, Reg.result),
        MFHI(Reg.result)
    )
    lazy val divideUnsigned: Code = block(
        DIVU(Reg.scratch, Reg.result),
        MFLO(Reg.result)
    )
    lazy val remainderUnsigned: Code = block(
        DIVU(Reg.scratch, Reg.result),
        MFHI(Reg.result)
    )

    /* The following `Code`s are intended to be used as the `comp` argument to `IfStmt`.
     * They should expect two operands in `Reg.scratch` and `Reg.result`, interpret them as two's-complement
     * signed integers (except `gtUnsignedCmp`), compare them, and branch to `label` if the comparison fails.
     *
     * The generated code may modify the values of Reg.result and Reg.scratch. If you
     * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
     * must not modify the values of any other registers that are already listed in Reg.scala.
     */
    def eqCmp(label: Label): Code = bne(Reg.scratch, Reg.result, label)
    def neCmp(label: Label): Code = beq(Reg.scratch, Reg.result, label)
    def ltCmp(label: Label): Code = block(
        SLT(Reg.result, Reg.scratch, Reg.result),
        beq(Reg.result, Reg.zero, label)
    )
    def gtCmp(label: Label): Code = block(
        SLT(Reg.result, Reg.result, Reg.scratch),
        beq(Reg.result, Reg.zero, label)
    )
    def leCmp(label: Label): Code = block(
        SLT(Reg.result, Reg.result, Reg.scratch),
        bne(Reg.result, Reg.zero, label)
    )
    def geCmp(label: Label): Code = block(
        SLT(Reg.result, Reg.scratch, Reg.result),
        bne(Reg.result, Reg.zero, label)
    )
    def gtUnsignedCmp(label: Label): Code = block(
        SLTU(Reg.result, Reg.result, Reg.scratch),
        beq(Reg.result, Reg.zero, label)
    )

    /** Generates code that evaluates `expr` to yield a memory address, then loads the word from that address
     * into `Reg.result`.
     *
     * The generated code may modify the values of Reg.result and Reg.scratch. If you
     * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
     * must not modify the values of any other registers that are already listed in Reg.scala.
     **/
    def deref(expr: Code): Code = block(
        expr,
        LW(Reg.result, 0, Reg.result)
    )

    /** Generates code that evaluates `target` to yield a memory address, then evaluates `expr` to yield a value,
     * then stores the value into the memory address.
     *
     * The generated code may modify the values of Reg.result and Reg.scratch. If you
     * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
     * must not modify the values of any other registers that are already listed in Reg.scala.
     */
    def assignToAddr(target: Code, expr: Code): Code = {
        val address = Variable("address")
        Scope(Seq[Variable](address), block(
            target,
            write(address, Reg.result),
            expr,
            read(Reg.scratch, address),
            SW(Reg.result, 0, Reg.scratch)
        ))

    }
    

    /** Generates code that implements a while loop. The generated code should evaluate `e1` and `e2`,
     * compare them using `comp`, and if the comparison succeeds, it should execute `body` and repeat
     * the entire process from the beginning.
     *
     * The generated code may modify the values of Reg.result and Reg.scratch. If you
     * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
     * must not modify the values of any other registers that are already listed in Reg.scala.
     */
    def whileLoop(e1: Code, comp: Label=>Code, e2: Code, body: Code): Code = {
        val loopStart = Label("loopStart")
        val afterLoop = Label("afterLoop")
        block(
            Comment("Start of loop"),
            Define(loopStart),
            ifStmt(e1, comp, e2, block(Comment("Loop condition passed")), beq(Reg.zero, Reg.zero, afterLoop)),
            Comment("Loop body"),
            body,
            beq(Reg.zero, Reg.zero, loopStart),
            Comment("End of loop"),
            Define(afterLoop)
        )
    }
    
    def abs(expr: Code): Code = {
        ifStmt(expr, geCmp, binOp(literal(0), minus, expr),
            expr,
            binOp(literal(0), minus, expr)
        )
    }
    
    def varExpr(variable: Variable): VarAccess = read(Reg.result, variable)
    def regExpr(reg: Reg): Code = ADD(Reg.result, reg)

    def print(code: Code): Code = assignToAddr(
        block(LIS(Reg.result), CPU.printAddr),
        code
    )
    
    def showReg(reg: Reg): Code = block(
        Comment(s"Showing ${reg.number}"),
        ADD(Reg.show, reg)
    )
    
    def literal(num: Int): Code = block(
        Comment(s"literal(${num})"),
        LIS(Reg.result),
        Word(encodeSigned(num))
    )

    val crash: Code = LW(Reg.result, -1, Reg.zero)

    def assertExprs(name: String, e1: Code, comp: Label=>Code, e2: Code): Code = block(
        Comment(s"${name} Asserting"),
        ADD(Reg.assert1, Reg.result),
        ADD(Reg.assert2, Reg.scratch),
        ifStmt(e1, comp, e2, block(), block(Comment(s"Assertion ${name} failed"), crash)),
        ADD(Reg.result, Reg.assert1),
        ADD(Reg.scratch, Reg.assert2)
    )
}
