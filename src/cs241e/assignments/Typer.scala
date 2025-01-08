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
import cs241e.scanparse.Grammars.*

import scala.collection.mutable

/** Implementation of context-sensitive analysis for the Lacs language. */

object Typer {
    /** Representation of a Lacs type, which is either an Int or a function type with parameter types and a return type.
     */
    sealed abstract class Type
    case object IntType extends Type
    case class FunctionType(parameterTypes: Seq[Type], returnType: Type) extends Type

    /** Given a `tree`, finds all descendants of the `tree` whose root node has kind `lhsKind`.
     * Does not search within the found subtrees for any nested occurrences of additional descendants.
     *
     * For example, searching the root of a program tree with `lhsKind = "procedure"` will return the trees all
     * of the top-level procedures, but not any procedures nested within them.
     */
    def collect(tree: Tree, lhsKind: String): Seq[Tree] =
        if(tree.lhs.kind == lhsKind) Seq(tree) else tree.children.flatMap((tree: Tree) => collect(tree, lhsKind))

    /** Given a tree that is either a "type" or contains exactly one "type" nested within it, returns
     * an instance of `Type` representing the corresponding type.
     */
    def parseType(tree: Tree): Type = {
        val types = collect(tree, "type")
        require(types.size == 1)

        val typeTree = types.head
        if (typeTree.children.head.lhs.kind == "INT") IntType
        else {
            require(typeTree.children.size == 5)
            val parameterTypes = collect(typeTree.children(1), "type").map(parseType)
            val returnType = parseType(typeTree.children(4))
            FunctionType(parameterTypes, returnType)
        }
    }

    /** A variable combined with its declared type. */
    case class TypedVariable(variable: Variable, tpe: Type)

    /** Create a new `Variable` given its `name` and type `tpe`. */
    def makeVariable(name: String, tpe: Type): Variable =
        new Variable(name, isPointer = (tpe != IntType))

    /** A `SymbolTable` maps each name to either a `TypedVariable` or a `ProcedureScope`. */
    type SymbolTable = Map[String, TypedVariable|ProcedureScope]

    /** Given a tree containing subtrees rooted at "vardef", creates a `TypedVariable` for each such tree. */
    def parseVarDefs(tree: Tree): Seq[TypedVariable] = {
        collect(tree, "vardef").map(varDefTree => {
            require(varDefTree.children.size == 3)
            val tpe = parseType(varDefTree.children(2))
            val variable = makeVariable(varDefTree.children.head.lhs.lexeme, tpe)
            // println(variable)
            TypedVariable(variable, tpe)
        })
    }

    /** Call `sys.error()` if any `String` occurs in `names` multiple times. */
    def checkDuplicates(names: Seq[String]): Unit = {
        // println(names)
        val duplicates = names.diff(names.distinct)
        if(duplicates.nonEmpty) sys.error(s"Duplicate identifiers ${duplicates}")
    }

    /** A `ProcedureScope` holds the semantic information about a particular procedure that is needed to type-check
     * the body of the procedure, including information coming from outer procedure(s) within which this
     * procedure may be nested.
     *
     * @param tree the tree defining the procedure (rooted at a "defdef")
     * @param outer the `ProcedureScope` of the outer procedure that immediately contains this one
     */
    class ProcedureScope(tree: Tree, outer: Option[ProcedureScope] = None) {
        assert(tree.production ==
            "defdef DEF ID LPAREN parmsopt RPAREN COLON type BECOMES LBRACE vardefsopt defdefsopt expras RBRACE")
        val Seq(_, id, _, parmsopt, _, _, retTypeTree, _, _, vardefs, defdefs, expras, _) = tree.children

        /** The name of the procedure. */
        val name: String = id.lhs.lexeme

        /** The parameters of the procedure. */
        val parms: Seq[TypedVariable] = parseVarDefs(parmsopt)

        /** The variables declared in the procedure. */
        val variables: Seq[TypedVariable] = parseVarDefs(vardefs)

        /** The declared return type of the procedure. */
        val returnType: Type = parseType(retTypeTree)

        /** The type of the procedure. */
        val tpe: FunctionType = FunctionType(parms.map(_.tpe), returnType)

        /** The new `Procedure` object that will represent this procedure. */
        val procedure: Procedure = new Procedure(name, parms.map(_.variable), outer.map(_.procedure))

        /** The `ProcedureScope`s of the nested procedures that are immediately nested within this procedure.
         *
         * Note: this `val` will recursively call `new ProcedureScope(...)`.
         */
        val subProcedures: Seq[ProcedureScope] = collect(defdefs, "defdef").map(new ProcedureScope(_, Some(this)))

        /** The names of parameters, variables, and nested procedures that are newly defined within this procedure
         * (as opposed to being inherited from some outer procedure).
         */
        val newNames: Seq[String] =
            parms.map(_.variable.name) ++ variables.map(_.variable.name) ++ subProcedures.map(_.procedure.name)
        checkDuplicates(newNames)

        /** Create and return a symbol table to be used when type-checking the body of this procedure. It
         * should contain all symbols (parameters, variables, nested procedures) defined in this procedure,
         * as well as those defined in outer procedures within which this one is nested. Symbols defined in
         * this procedure override (shadow) those of outer procedures. The `outerSymbolTable` parameter
         * contains the symbol table of the enclosing scope (either an outer procedure within which the
         * current procedure is nested, or, if the current procedure is a top-level procedure, a symbol
         * table containing the names of all of the top-level procedures).
         */
        def symbolTable(outerSymbolTable: SymbolTable): SymbolTable = {
            var updatedMap = outerSymbolTable
            updatedMap = updatedMap ++ parms.map(parm => parm.variable.name -> parm)
            updatedMap = updatedMap ++ variables.map(variable => variable.variable.name -> variable)
            updatedMap ++ subProcedures.map(subProcedure => subProcedure.procedure.name -> subProcedure)
        }

        /** Returns a sequence containing `this` `ProcedureScope` and the `ProcedureScope`s for all procedures
         * declared inside of this procedure, including those nested recursively within other nested procedures.
         *
         * Scala hint: learn about the `flatMap` method in the Scala library. If you are not familiar with flatMap,
         * one place you can read about it is here:
         * http://www.artima.com/pins1ed/working-with-lists.html#sec:higher-order-methods
         */
        def descendantScopes: Seq[ProcedureScope] = this +: subProcedures.flatMap(_.descendantScopes)

        override def toString = s"ProcedureScope for $name"
    }

    /** Creates a map containing a symbol table for each procedure scope by calling the scope's symbolTable method,
     * passing in the symbol table of its outer enclosing procedure (or the top level symbol table for a top level
     * procedure).
     */
    def createSymbolTables(topLevelProcedureScopes: Seq[ProcedureScope], topLevelSymbolTable: SymbolTable):
    Map[ProcedureScope, SymbolTable] = {
        def recur(procedureScopes: Seq[ProcedureScope], outerSymbolTable: SymbolTable): Map[ProcedureScope, SymbolTable] = {
            procedureScopes.flatMap{ procedureScope =>
                val symbolTable = procedureScope.symbolTable(outerSymbolTable)
                Map(procedureScope -> symbolTable) ++ recur(procedureScope.subProcedures, symbolTable)
            }.toMap
        }
        recur(topLevelProcedureScopes, topLevelSymbolTable)
    }

    /** Checks that the body of a procedure satisfies the type-checking rules in the Lacs language specification.
     * Returns a `Map` that provides a `Type` for each `Tree` that has a `Type` according to the language
     * specification.
     */

    def typeCheck(scope: ProcedureScope, symbolTable: SymbolTable): Map[Tree, Type] = {
        /** The map that will be returned containing the `Type` of each `Tree` that has a `Type`. */
        val treeToType = mutable.Map[Tree, Type]()

        /** Calls `sys.error()` if `tpe1` and `tpe2` are not equal. If they are equal, returns them. */
        def mustEqual(tpe1: Type, tpe2: Type): Type =
            if(tpe1 == tpe2) tpe1 else sys.error(s"Type mismatch: expected $tpe2, got $tpe1")

        /** For a `tree` rooted at a node that has a `Type`, computes the `Type`, adds it to `treeToType`,
         * and returns it.
         *
         * Calls `sys.error()` if the `tree` does not conform to the typing rules in the Lacs specification.
         */
        def err(kind: String) = sys.error(s"$kind type not in symbol table")
        def typeOf(tree: Tree): Type = {
            def fun: Type = {
                val kind = tree.lhs.kind
                kind match
                    case "ID" => symbolTable.getOrElse(tree.lhs.lexeme, err(kind)) match {
                        case variable: TypedVariable => variable.tpe
                        case scope: ProcedureScope => scope.tpe
                    }
                    case "factor" => tree.children.head.lhs.kind match {
                        case "ID" => typeOf(tree.children.head)
                        case "NUM" => IntType
                        case "LPAREN" => typeOf(tree.children(1))
                        case "factor" => typeOf(tree.children.head) match {
                            case f: FunctionType =>
                                if (f.parameterTypes == collect(tree.children(2), "expr").map(typeOf))
                                    f.returnType
                                else sys.error("factor: factor params and args do not match")
                            case _ => err("factor: child factor is not procedure type")
                        }
                    }
                    case "term" => tree.children.head.lhs.kind match {
                        case "factor" => typeOf(tree.children.head)
                        case "term" =>
                            if (typeOf(tree.children.head) == IntType && typeOf(tree.children.last) == IntType) IntType
                            else sys.error("term: child term or factor are not Int")
                    }
                    case "expr" => tree.children.head.lhs.kind match {
                        case "term" => typeOf(tree.children.head)
                        case "expr" =>
                            if (typeOf(tree.children.head) == IntType && typeOf(tree.children.last) == IntType)
                                IntType
                            else sys.error("expr: child term or expr is not Int")
                        case "IF" =>
                            val testTree = tree.children(2)
                            if (typeOf(testTree.children.head) != IntType) sys.error("expr if: expr1 is not Int")
                            else if (typeOf(testTree.children.last) != IntType) sys.error("expr if: expr2 is not Int")
                            else if (typeOf(tree.children(5)) == typeOf(tree.children(9))) typeOf(tree.children(5))
                            else sys.error("expr if: expras are not same")
                    }
                    case "expra" =>
                        // println(scope.name)
                        // println(tree)
                        // println(typeOf(tree.children.head))
                        tree.children.head.lhs.kind match {
                        case "ID" =>
                            if (typeOf(tree.children.head) == typeOf(tree.children.last)) {
                                symbolTable.getOrElse(tree.children.head.lhs.lexeme, err("ID")) match
                                    case _: TypedVariable => typeOf(tree.children.last)
                                    case _ => sys.error("expra: ID is not variable")
                            }
                            else sys.error("expra: ID and expr are not same")
                        case "expr" => typeOf(tree.children.last)
                    }
                    case "expras" =>
                        var res = typeOf(tree.children.head)
                        if (tree.children.size == 3) res = typeOf(tree.children.last)
                        res
                    case "defdef" => symbolTable.getOrElse(tree.lhs.lexeme, err("defdef")) match {
                        case procedureScope: ProcedureScope =>
                            if (typeOf(tree.children(6)) == typeOf(tree.children.last)) procedureScope.tpe
                            else sys.error("defef: expras and type are not same")
                        case _ => err("defdef")
                    }
            }
            treeToType.getOrElseUpdate(tree, fun)
        }

        /* Check that the type of the expression returned from the procedure matches the declared type of the procedure. */
        mustEqual(scope.returnType, typeOf(scope.expras))

        Map() ++ treeToType
    }

    /** A data structure representing the result of context-sensitive analysis of a whole Lacs program.
     *
     * @param procedureScopes the `ProcedureScopes` representing the semantic information about each procedure.
     * @param symbolTables a symbol table for each procedure in the program.
     * @param typeMap result of type-checking: provides a type for each tree node that represents an expression.
     */
    case class TypedProcedures(
                                  procedureScopes: Seq[ProcedureScope],
                                  symbolTables: ProcedureScope=>SymbolTable,
                                  typeMap: PartialFunction[Tree,Type]) {

        /** Output human-readable form of a parse tree (for debugging purposes) annotated with the type information
         * for tree nodes that have a type.
         **/
        def showTree(tree: Tree, indent: Int = 0): String = {
            val typeString = typeMap.lift.apply(tree) match {
                case Some(tpe) => ": " + tpe
                case None => ""
            }
            " " * indent + tree.lhs + typeString + "\n" +
                tree.children.map(ch => showTree(ch, indent+1)).mkString
        }

        override def toString = procedureScopes.map{ procedureScope =>
            procedureScope.toString + "\n" +
                "Symbol table:\n" +
                symbolTables(procedureScope).map{case (name, meaning) => s" $name -> $meaning\n"}.mkString +
                "Procedure body with types:\n" +
                showTree(procedureScope.expras)
        }.mkString("\n")
    }

    /** Type-checks a Lacs program parse tree. Returns `TypedProcedures`, which contains the `ProcedureScope`s
     * representing the procedures, a map giving a `SymbolTable` for each `ProcedureScope`,
     * and a map giving the `Type` of each `Tree` that has one.
     */
    def typeTree(tree: Tree): TypedProcedures = {
        assert(tree.production == "S BOF defdefs EOF")
        val defdefs = tree.children(1)

        val topLevelProcedureScopes = collect(defdefs, "defdef").map{defdef => new ProcedureScope(defdef, None)}
        checkDuplicates(topLevelProcedureScopes.map(procedureScope => procedureScope.name))
        val topLevelSymbolTable: SymbolTable =
            topLevelProcedureScopes.map{procedure => (procedure.name -> procedure)}.toMap
        val symbolTables = createSymbolTables(topLevelProcedureScopes, topLevelSymbolTable)

        val allProcedureScopes = topLevelProcedureScopes.flatMap(procedureScope => procedureScope.descendantScopes)

        val typeMap: Map[Tree, Type] = allProcedureScopes.flatMap(procedureScope =>
            typeCheck(procedureScope, symbolTables(procedureScope))).toMap

        val mainProcedure = topLevelProcedureScopes.head
        if(mainProcedure.tpe != FunctionType(Seq(IntType, IntType), IntType))
            sys.error("The type of the main procedure must be (Int, Int)=>Int.")

        TypedProcedures(allProcedureScopes, symbolTables, typeMap)
    }
}
