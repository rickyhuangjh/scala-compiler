import cs241e.assignments.*
import cs241e.assignments.Assembler.*
import cs241e.assignments.CodeBuilders.*
import cs241e.assignments.MemoryManagement.*
import cs241e.assignments.ProgramRepresentation.*
import cs241e.assignments.Transformations.*
import cs241e.mips.*
import org.scalatest.funsuite.AnyFunSuite

class A7TestsL1 extends AnyFunSuite {
    test("test") {
        val input = "123123"
        val dfa = A7.decimalNumber
        val tokens = Scanning.maximalMunchScan(dfa, input)
        codePrinter.pprintln(tokens)
    }
    test("recognize") {
        assert(Scanning.recognize(A7.notDiv3, "11".toList) == false)
        assert(Scanning.recognize(A7.notDiv3, "100".toList) == true)
    }
    test("scan") {
        val tokens3 = Lacs.scan("")
        codePrinter.pprintln(tokens3)
        assertThrows[RuntimeException](Lacs.scan("3x"))
        assertThrows[RuntimeException](Lacs.scan("!"))
        assertThrows[RuntimeException](Lacs.scan("===="))
    }
}
class A7TestsL2 extends AnyFunSuite {
    test("recognize") {
        println(Scanning.recognize(A7.notDiv3, "11".toList))
        println(Scanning.recognize(A7.notDiv3, "100".toList))
    }
    test("scan") {
        val prog = "def main(a: Int, b: Int): Int = { a + b }"
        val tokens = Scanning.maximalMunchScan(Lacs.dfa, prog)
        codePrinter.pprintln(tokens)
        val tokens2 = Lacs.scan(prog)
        codePrinter.pprintln(tokens2)
        codePrinter.pprintln(Scanning.maximalMunchScan(Lacs.dfa, "3x"))
        assertThrows[RuntimeException](Lacs.scan("3x"))
        assertThrows[RuntimeException](Lacs.scan("!"))
        assertThrows[RuntimeException](Lacs.scan("===="))
    }
}
