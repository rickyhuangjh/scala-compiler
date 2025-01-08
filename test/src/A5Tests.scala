import cs241e.assignments.*
import cs241e.assignments.Assembler.*
import cs241e.assignments.CodeBuilders.*
import cs241e.assignments.MemoryManagement.*
import cs241e.assignments.ProgramRepresentation.*
import cs241e.assignments.Transformations.*
import cs241e.mips.*
import org.scalatest.funsuite.AnyFunSuite

class A5TestsL1 extends AnyFunSuite {
    test("scala") {
        def main(a: Int, b: Int): Int = { a + b }
        println(main(1,2))
    }
    test("lacs") {
        val a = new Variable("a")
        val b = new Variable("b")
        val main = new Procedure("main", Seq(a,b))
        main.code = binOp(read(Reg.result, a), plus, read(Reg.result, b))
        val machineCode = compilerA5(Seq(main))
        val endState = A4.loadAndRun(machineCode, register1 = Word(encodeSigned(1)),
            register2 = Word(encodeSigned(2)), debug = false)
        println(decodeSigned(endState.reg(3)))
    }
    test("scala_fact") {
        def main(a: Int, b: Int): Int = { fact(a) }
        def fact(i: Int): Int = {
            val iMinusOne = i-1
            if(i<=0) 1 else i*fact(iMinusOne)
        }
        println(main(10,2))
    }
    def const(i: Int) = block(
        LIS(Reg.result),
        Word(encodeSigned(i))
    )
    test("lacs_fact") {
        val a = new Variable("a")
        val b = new Variable("b")
        val main = new Procedure("main", Seq(a,b))
        val i = new Variable("i")
        val iMinusOne = new Variable("iMinusOne")
        val fact = new Procedure("fact", Seq(i))
        main.code = call(fact, read(Reg.result, a))
        fact.code = Scope(Seq(iMinusOne), block(
            assign(iMinusOne, binOp(read(Reg.result, i), minus, const(1))),
            ifStmt(read(Reg.result, i), leCmp, const(0), const(1),
                binOp(read(Reg.result, i), times, call(fact, read(Reg.result, iMinusOne))))
            // test: f(g())
            // test: reading/writing variables and parameters
        ))
        val machineCode = compilerA6(Seq(main, fact))
        val endState = A4.loadAndRun(machineCode, register1 = Word(encodeSigned(10)),
            register2 = Word(encodeSigned(2)), debug = false)
        println(decodeSigned(endState.reg(3)))
    }
    
    test("treeheight") {
        val array = Seq(77, 3, 6, 22, -1, -1, -8, 9, 12, -36, -1, -1, 999, -1, -1)
            .map(x => Word(encodeSigned(x)))
        val machineCode = compilerA6(A5.treeHeight)
        val endState = A4.loadAndRunArray(machineCode, array, debug = false)
        println(decodeSigned(endState.reg(3)))
    }
}