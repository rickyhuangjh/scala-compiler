import cs241e.assignments.*
import org.scalatest.funsuite.AnyFunSuite

class A9TestsL1 extends AnyFunSuite {
    test("test") {
        val prog ="def main(a: Int, b: Int): Int = { var x: (Int, Int) => Int; x = main; 0}"
        val tree = Lacs.scanAndParse(prog)
        // println(tree)
        val typedProcedures = Typer.typeTree(tree)
        // println(typedProcedures)
    }
    test("test2") {
        def good(prog: String) = Lacs.scanAndParseAndType(prog)
        def bad(prog: String) = {
            try {
                val tree = Lacs.scanAndParse(prog)
                // println(tree)
                Typer.typeTree(tree)
            } catch { e => println(e) }
            println(intercept[RuntimeException](Lacs.scanAndParseAndType(prog)))
            assertThrows[RuntimeException](Lacs.scanAndParseAndType(prog))
        }
        // good("def main(a: Int, b: Int): Int = { a + b }")
        bad("def main(a: Int, b: Int): Int = { var x: (Int, Int) => Int; x = main; 0}")
        // bad("def main(a: Int, b: Int): Int = { a + c }")
    }
}
