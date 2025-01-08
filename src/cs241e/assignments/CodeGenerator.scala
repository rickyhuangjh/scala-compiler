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

import cs241e.*
import ProgramRepresentation.*
import CodeBuilders.*
import Typer.*
import scanparse.Grammars.*

/** A code generator that converts a Lacs parse tree into the intermediate language developed in Assignment 1 to 6. */
object CodeGenerator {
    def generateProcedures(typedProcedures: TypedProcedures) = {

        /** Given a `procedureScope`, generates the `Code` implementing the procedure, and assigns it to
         * `procedureScope.procedure.code`.
         */
        def generateCode(procedureScope: ProcedureScope): Unit = {

            val symbolTable = typedProcedures.symbolTables(procedureScope)

            /** Generates the `Code` that implements `tree`.
             *
             * This method will be called from the outside only on `tree`s rooted at nodes of kind "expras".
             * However, if it calls itself recursively on other kinds of nodes, it needs to be able to handle
             * those node kinds as well.
             */
            def generateArgs(argsTree: Tree): Seq[Code] = {
                if (argsTree.children.size == 1) Seq(recur(argsTree.children.head))
                else recur(argsTree.children.head) +: generateArgs(argsTree.children.last)
            }

            def recur(tree: Tree): Code = tree.lhs.kind match
                case "factor" => tree.children.head.lhs.kind match
                    // rvalue
                    case "ID" => symbolTable(tree.children.head.lhs.lexeme) match {
                        case tv: TypedVariable => varExpr(tv.variable)
                        case ps: ProcedureScope => Closure(ps.procedure)
                    }
                    case "NUM" => literal(tree.children.head.lhs.lexeme.toInt)
                    case "LPAREN" => recur(tree.children(1))
                    case "factor" => {
                        val childFactorTree = tree.children.head
                        val argsoptTree = tree.children(2)
                        val args = {
                            if (argsoptTree.children.isEmpty) Seq.empty[Code]
                            else generateArgs(argsoptTree.children.head)
                        }
                        val childFactorChildTree = childFactorTree.children.head
                        if (childFactorChildTree.lhs.kind == "ID") {
                            symbolTable(childFactorChildTree.lhs.lexeme) match
                                case ps: ProcedureScope => Call(ps.procedure, args)
                                case _ => {
                                    val paramTypes = typedProcedures.typeMap(childFactorTree) match
                                        case f: FunctionType => f.parameterTypes
                                        case _ => sys.error("uh oh")
                                    val dummyParams = paramTypes.map(makeVariable("", _))
                                    CallClosure(recur(childFactorTree), args, dummyParams)
                                }
                        } else {
                            val paramTypes = typedProcedures.typeMap(childFactorTree) match {
                                case f: FunctionType => f.parameterTypes
                                case _ => sys.error("uh oh")
                            }
                            val dummyParams = paramTypes.map(makeVariable("", _))
                            CallClosure(recur(childFactorTree), args, dummyParams)
                        }
                    }
                case "term" =>
                    if (tree.children.size == 1) recur(tree.children.head)
                    else tree.children(1).lhs.kind match {
                        case "STAR" => binOp(recur(tree.children.head), times, recur(tree.children.last))
                        case "SLASH" => binOp(recur(tree.children.head), divide, recur(tree.children.last))
                        case "PCT" => binOp(recur(tree.children.head), remainder, recur(tree.children.last))
                        case _ => sys.error("term uh oh")
                    }
                case "expr" =>
                    if (tree.children.size == 1) recur(tree.children.head)
                    else if (tree.children.size == 3) tree.children(1).lhs.kind match {
                        case "PLUS" => binOp(recur(tree.children.head), plus, recur(tree.children.last))
                        case "MINUS" => binOp(recur(tree.children.head), minus, recur(tree.children.last))
                    }
                    else {
                        val testTree = tree.children(2)
                        val e1 = recur(testTree.children.head)
                        val e2 = recur(testTree.children.last)
                        val comp = testTree.children(1).lhs.kind match {
                            case "NE" => neCmp
                            case "LT" => ltCmp
                            case "LE" => leCmp
                            case "GE" => geCmp
                            case "GT" => gtCmp
                            case "EQ" => eqCmp
                            case _ => sys.error("expr if oh no")
                        }
                        val thens = recur(tree.children(5))
                        val elses = recur(tree.children(9))
                        ifStmt(e1, comp, e2, thens, elses)
                    }
                case "expra" =>
                    if (tree.children.size > 1) {
                        symbolTable(tree.children.head.lhs.lexeme) match
                            case tv: TypedVariable => assign(tv.variable, recur(tree.children.last))
                            case _ => sys.error("expra oh no")
                    } else recur(tree.children.head)
                case "expras" =>
                    if (tree.children.size > 1) block(recur(tree.children.head), recur(tree.children.last))
                    else recur(tree.children.head)

            /* Main body of generateCode. */
            procedureScope.procedure.code = Scope(procedureScope.variables.map(_.variable), recur(procedureScope.expras))
        }
            /* Main body of generateProcedures. */

        typedProcedures.procedureScopes.foreach(generateCode)
        typedProcedures.procedureScopes.map(_.procedure)
    }
}
