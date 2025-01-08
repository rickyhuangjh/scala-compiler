import cs241e.assignments.*
import cs241e.assignments.Assembler.*
import cs241e.assignments.CodeBuilders.*
import cs241e.assignments.MemoryManagement.*
import cs241e.assignments.ProgramRepresentation.*
import cs241e.assignments.Transformations.*
import cs241e.mips.*
import org.scalatest.funsuite.AnyFunSuite
import cs241e.assignments.A5

class A6Tests extends AnyFunSuite {


    lazy val main: Seq[Procedure] = {

        val x = new Variable("x")
        val y = new Variable("y")

        val makeAdder = new Procedure("makeAdder", Seq(x))
        val f = new Procedure("f", Seq(y), Option(makeAdder))
        f.code = block(
            binOp(varExpr(x), plus, varExpr(y))
        )

        makeAdder.code = block(
            Closure(f)
        )

        val a = new Variable("a")
        val b = new Variable("b")
        val adderA = new Variable("adderA")
        val adderB = new Variable("adderB")
        val main = new Procedure("main", Seq(a, b))
        main.code = Scope(Seq(adderA, adderB), block(
            assign(adderA, call(makeAdder, varExpr(a))),
            assign(adderB, call(makeAdder, varExpr(b))),

            call(
                A5.printProcedure,
                CallClosure(
                    varExpr(adderA),
                    Seq(literal(5)),
                    Seq(new Variable(""))
                )
            ),
            call(
                A5.printProcedure,
                CallClosure(
                    varExpr(adderB),
                    Seq(literal(5)),
                    Seq(new Variable(""))
                )
            )
        ))
        Seq(main, makeAdder, f, A5.printProcedure)
    }



    test("closures") {
        val machineCode = compilerA6(main)
        A4.loadAndRun(machineCode, Word(encodeSigned(10)), Word(encodeSigned(20)), debug = true)
    }
}
