import cs241e.assignments.*
import cs241e.assignments.Assembler.*
import cs241e.assignments.CodeBuilders.binOp
import cs241e.assignments.MemoryManagement.*
import cs241e.assignments.ProgramRepresentation.*
import cs241e.assignments.Transformations.compilerA6
import cs241e.mips.*
import org.scalatest.funsuite.AnyFunSuite
import cs241e.assignments.Lacs

class A11TestsL1 extends AnyFunSuite {
    test("lacs") {
        println(CPU.maxAddr)
        def compileAndRun(prog: String, a: Int = 0, b: Int = 0, debug: Boolean = false) = {
            val machineCode = Lacs.compileWithGarbageCollector(prog)
            val finalState = A4.loadAndRun(machineCode, Word(encodeSigned(a)), Word(encodeSigned(b)), debug=debug)
            println(decodeSigned(finalState.reg(3)))
        }

        compileAndRun("def main(a: Int, b: Int): Int = { a + b }", 4, 7, debug = false)
        compileAndRun("def main(a: Int, b: Int): Int = { a * b }", 4, 7, debug = false)
        val prog =
            """
      def main(a: Int, b: Int): Int = {
        increaseBy(a)(b)
      }
      def increaseBy(increment: Int): (Int)=>Int = {
        def procedure(x: Int): Int = { x + increment }
        procedure
      }
        """
        compileAndRun(prog, 4, 7, debug = false)
        // GC runs only if
        // - program uses closures
        // - from-space is full
        // For testing, modify alocateProc to call collectGarbage on EVERY allocation
        // The "stress" Marmoset test does this
    }
    test("heaps") {
        def v(q: Variable) = {
            read(Reg.result, q)
        }

        withGC {
            val a = new Variable("a")
            val b = new Variable("b")
            val main = new Procedure("main", Seq(a,b))
            val f = new Variable("f", isPointer = true)
            val chunk = Chunk(Seq(f))
            def writeField(base: Code, field: Variable, value: Code) = {
                binOp(base, chunk.store(Reg.scratch, field, Reg.result), value)
            }

            val p = new Variable("p", isPointer = true)
            val q = new Variable("q", isPointer = true)
            main.code = Scope(Seq(p,q), block(
                assign(p, heap.allocate(chunk)),
                assign(q, heap.allocate(chunk)),
                writeField(v(q), f, v(p)),
                writeField(v(p), f, v(q)),
                assign(p, v(q)),
                call(GarbageCollector.collectGarbage),
                ADD(Reg.zero, Reg.zero, Reg.zero) // prevent tail call
            ))
            val machineCode = compilerA6(Seq(main) ++ GarbageCollector.procedures)
            val finalState = A4.loadAndRun(machineCode)
            def dumpMem(state: State, address: Word, words: Int = 6): Unit = {
                if(words>0) {
                    println(spaces(address) + ": " + spaces(state.mem(address)))
                    dumpMem(state, Word(encodeUnsigned(decodeUnsigned(address)+4)), words-1)
                }
            }
            def spaces(w: Word): String = w.toString().sliding(8,8).mkString(" ")

            println("Semispace 1:")
            dumpMem(finalState, GarbageCollector.heapStart)
            println("Semispace 2:")
            dumpMem(finalState, GarbageCollector.heapMiddle)
            println("heapPtr: " + spaces(finalState.reg(Reg.heapPointer.number)))
            println("reachable bytes: " + decodeSigned(finalState.reg(Reg.result.number)))

        }
    }
}