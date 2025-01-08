import cs241e.assignments.*
import cs241e.assignments.Assembler.{decodeSigned, encodeSigned}
import cs241e.assignments.Lacs.compileWithGarbageCollector
import cs241e.assignments.ProgramRepresentation.{codePrinter, disassemblingCodePrinter}
import cs241e.assignments.Transformations.compilerA6
import cs241e.mips.Word
import cs241e.assignments.MemoryManagement.GarbageCollector
import org.scalatest.funsuite.AnyFunSuite

class A10TestsL1 extends AnyFunSuite {
    test("test") {
        val prog =
            """
      def main(a: Int, b: Int): Int = { if((if (a < b) { mult } else { add })(a, b) > 20) { 1 } else { 2} }
      def mult(x: Int, y: Int): Int = { x * (y + 1) }
      def add(x: Int, y: Int): Int = { x + y + 1 }
      def foo(c: Int, d: (Int,Int)=>Int): Int = { d(c,c) }
      """
        println(GarbageCollector.heapStart)
        println(GarbageCollector.heapMiddle)
        println(GarbageCollector.heapEnd)
        val machineCode = compileWithGarbageCollector(prog)
        val finalState = A4.loadAndRun(machineCode, Word(encodeSigned(6)), Word(encodeSigned(5)), debug=true)
        println(decodeSigned(finalState.reg(3)))
    }
    test("lacs") {
        def compileAndRun(prog: String, a: Int = 0, b: Int = 0, debug: Boolean = false) = {
            val machineCode = Lacs.compile(prog)
            val finalState = A4.loadAndRun(machineCode, Word(encodeSigned(a)), Word(encodeSigned(b)), debug=false)
            println(decodeSigned(finalState.reg(3)))
        }

        compileAndRun("def main(a: Int, b: Int): Int = { a + b }", 4, 7, debug = false)
        compileAndRun("def main(a: Int, b: Int): Int = { a * b }", 4, 7, debug = false)
    }
}