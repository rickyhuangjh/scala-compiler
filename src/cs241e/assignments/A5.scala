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

import ProgramRepresentation.{assign, ifStmt, *}
import CodeBuilders.*
import cs241e.assignments.Assembler.*

object A5 {
    /** The code of `printInteger` from Assignment 4 encapsulated as a `Procedure`. The procedure should have
     * exactly one parameter, the integer to be printed. */
    lazy val printProcedure: Procedure = {
        val theInteger = Variable("theInteger")
        val params: Seq[Variable] = Seq[Variable](theInteger)
        val procedure = new Procedure("printProcedure", params)
        procedure.code = block(
            // You may add code here before and/or after `A4.printIntegerCode`. If you do not want any
            // code before or after, replace the ??? with an empty `block()`.
            Comment("Reading integer into Reg 1"),
            read(Reg(1), theInteger),
            A4.printIntegerCode,
            block()
        )
        procedure
    }

    /** This procedure will be executed with an array of 32-bit integers as in Assignment 4.
     * It should take two parameters: the first is the address of the beginning of the array
     * and the second is the number of elements in the array.
     * The procedure should call `printProcedure` for each integer in the array in turn,
     * to print all the integers, and return.
     *
     * Test this procedure by compiling it with `printProcedure` and running it on various arrays.
     */
    lazy val printArray: Procedure = {
        val arrayStartParam = new Variable("arrayStartParam")
        val arraySizeParam = new Variable("arraySizeParam")
        val params = Seq[Variable](arrayStartParam, arraySizeParam)
        val idx = new Variable("idx")
        val curAddress = new Variable("curAddress")
        val curVal = new Variable("curVal")
        val procedure = new Procedure("printArray", params)
        val here = Label("here")

        procedure.code = Scope(Seq[Variable](idx, curAddress, curVal), block(
            assign(idx, literal(0)),
            whileLoop(varExpr(idx), ltCmp, varExpr(arraySizeParam), block(
                assign(curAddress, binOp(varExpr(arrayStartParam), plus,
                    binOp(varExpr(idx), times, literal(4))
                )),
                assign(curVal, deref(varExpr(curAddress))),
                Comment("Calling printProcedure"),
                call(printProcedure, varExpr(curVal)),
                Comment("Call to printProcedure completed"),
                assign(idx, binOp(varExpr(idx), plus, literal(1)))
            ))
        ))
        procedure
    }

    /** This procedure will be executed with an array of 32-bit integers as in Assignment 4.
     * It should take two parameters: the first is the address of the beginning of the array
     * and the second is the number of elements in the array.
     *
     * You may use multiple procedures if you wish. Generate them and return them in a `Seq`.
     * The tests will execute the first procedure in the sequence.
     *
     * The task is to determine the height of a binary tree and return it (in `Reg.result`).
     * Assume that every tree contains at least one node and hence has a height of at least one.
     * Each node of the tree is encoded in three consecutive elements (words) of the array:
     * a two's-complement integer stored at the node, the node's left child, and the node's right child.
     * Each child is specified as the array index of the first element of the child node.
     * The integer -1 indicates that a node does not have a left or right child. For example, the following tree:
     *
     *   77
     *  /  \
     * 22    -8
     *     /  \
     *   -36   999
     *
     * could be encoded by following array:
     *
     * A[0] = 77
     * A[1] = 3
     * A[2] = 6
     * A[3] = 22
     * A[4] = -1
     * A[5] = -1
     * A[6] = -8
     * A[7] = 9
     * A[8] = 12
     * A[9] = -36
     * A[10] = -1
     * A[11] = -1
     * A[12] = 999
     * A[13] = -1
     * A[14] = -1
     *
     * in which the root is encoded by the elements A[0], A[1] and A[2], the root's left child is encoded
     * by the elements A[3], A[4] and A[5], the root's right child is encoded by the elements A[6], A[7] and A[8],
     * the root's left-most grandchild is encoded by the elements A[9], A[10] and A[11],
     * and the root's right-most grandchild is encoded by the elements A[12], A[13] and A[14].
     *
     * This example tree has height 3.
     */
    lazy val max: Procedure = {
        val a = new Variable("a")
        val b = new Variable("b")
        val max = new Procedure("max", Seq(a, b))
        val res = new Variable("res")
        max.code = Scope(Seq(res), block(
            ifStmt(varExpr(a), gtCmp, varExpr(b),
                assign(res, varExpr(a)),
                assign(res, varExpr(b)),
            ),
            varExpr(res)
        ))
        max
    }

    lazy val recurse: Procedure = {
        val array = new Variable("array")
        val length = new Variable("length")
        val idx = new Variable("idx")
        val params = Seq(array, length, idx)


        val leftIdx = new Variable("leftIdx")
        val rightIdx = new Variable("rightIdx")
        val leftRes = new Variable("leftRes")
        val rightRes = new Variable("rightRes")
        val res = new Variable("res")
        val recurse = new Procedure("recurse", params)
        recurse.code = Scope(Seq(leftIdx, rightIdx, leftRes, rightRes, res), block(
            assign(res, literal(0)),
            ifStmt(varExpr(idx), geCmp, literal(0), block(
                assign(res, literal(1)),
                assign(leftIdx, binOp(varExpr(idx), plus, literal(1))),
                assign(leftRes, call(recurse, varExpr(array), varExpr(length), deref(
                    binOp(varExpr(array), plus, binOp(varExpr(leftIdx), times, literal(4)))
                ))),
                assign(rightIdx, binOp(varExpr(idx), plus, literal(2))),
                assign(rightRes, call(recurse, varExpr(array), varExpr(length), deref(
                    binOp(varExpr(array), plus, binOp(varExpr(rightIdx), times, literal(4)))
                ))),
                assign(res, binOp(varExpr(res), plus,
                    call(max, varExpr(leftRes), varExpr(rightRes)))
                ),
            )),
            varExpr(res)
        ))
        recurse
    }

    lazy val treeHeight: Seq[Procedure] = {
        val array = new Variable("array")
        val length = new Variable("length")
        val treeHeight = new Procedure("treeHeight", Seq(array, length))

        treeHeight.code = call(recurse, varExpr(array), varExpr(length), literal(0))

        /* You may, but are not required to, define and use more procedures in addition to `treeHeight`.
         * `treeHeight` must be the first procedure in the sequence that is returned.
         */
        Seq(treeHeight, recurse, max)
    }
}
