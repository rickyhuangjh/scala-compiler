import cs241e.assignments.Assembler.*
import cs241e.assignments.CodeBuilders.*
import cs241e.assignments.MemoryManagement.{Chunk, Stack}
import cs241e.assignments.ProgramRepresentation.*
import cs241e.assignments.Transformations.*
import cs241e.assignments.{A1, A4, A5, Assembler, Debugger, Reg}
import cs241e.mips.*
import org.scalatest.funsuite.AnyFunSuite

class A4Tests extends AnyFunSuite {
    test("test") {
        val array = Seq.tabulate(10)(i => Word(encodeSigned(i)))
        A4.loadAndRunArray(A4.outputLetters, array, debug = true)
    }

    test("scala") {
        def proc() = {
            var v = 5
            if (1 + 1 == 2) v = 2 * (3 + v) else v = 42
            v
        }

        println(proc())
    }

    def const(i: Int) = block(
        LIS(Reg.result),
        Word(encodeSigned(i))
    )

    test("lacs") {
        val v = new Variable("v")
        val code = Scope(Seq(v), block(
            assign(v, const(5)), // v = 5
            ifStmt(binOp(const(1), plus, const(1)), eqCmp, const(2),
                assign(v, binOp(const(2), times, binOp(const(3), plus, read(Reg.result, v)))),
                assign(v, const(42))
            ),
            read(Reg.result, v)
        ))
        val machineCode = compilerA4(code)
        val finalState = A4.loadAndRun(machineCode, debug = true)
        println(decodeSigned(finalState.reg(3)))
    }
    test("arrayScala") {
        val array = Array(2, 4, 6, 8)
        println(array(2))
    }
    test("arrayLacs") {
        val array = Seq[Word](
            Word(encodeSigned(2)),
            Word(encodeSigned(4)),
            Word(encodeSigned(6)),
            Word(encodeSigned(8)),
        )
        val arrayAddr = new Variable("arrayAddr")
        val size = new Variable("size")
        val code = Scope(Seq(arrayAddr, size), block(
            write(arrayAddr, Reg(1)),
            write(size, Reg(2)),

            Comment("now reading array(2)"),
            deref(binOp(read(Reg.result, arrayAddr), plus, binOp(const(4), times, const(2))))
        ))
        val machineCode = compilerA4(code)
        val finalState = A4.loadAndRunArray(machineCode, array, debug = true)
        println(decodeSigned(finalState.reg(3)))
    }

    test("test2") {
        val array = Seq.tabulate(10)(i => Word(encodeSigned(i + 1)))
        val machineCode = compilerA5(Seq(A5.printArray, A5.printProcedure))
        // machineCode.words.foreach(i => println(Debugger.disassemble(i)))
        A4.loadAndRunArray(machineCode, array, debug = false)
    }


}
