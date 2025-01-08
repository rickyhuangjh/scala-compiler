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

import cs241e.assignments
import cs241e.assignments.Assembler.*
import cs241e.assignments.CodeBuilders.*
import cs241e.assignments.ProgramRepresentation.*
import cs241e.mips.*
import jdk.jfr.Unsigned

/** An implementation of a strategy to lay out and keep track of memory, both on the stack and on the heap. */

object MemoryManagement {

    /** To make it easier to keep track of which areas of memory are used to store which data, we
     * organize all memory in `Chunk`s. A `Chunk` is a compile-time representation of multiple
     * words of memory indexed by a sequence of `Variable`s. Each variable is assigned a fixed
     * offset from the beginning of the `Chunk`. The `Chunk` can generate the
     * code to store and load the value at the offset correxponding to each variable.
     *
     * When a `Chunk` is represented in memory at run time, word 0 of the `Chunk`
     * always holds the size of the `Chunk` in bytes. This makes it possible to deallocate
     * or copy the run-time instance of the `Chunk` in memory without knowing the specific
     * static `Chunk` that it represents. Word 1 of the `Chunk` is reserved for Assignment 11
     * (discussed in the next paragraph). To summarize, the memory layout looks as follows:
     * word 0: size in bytes (4 * (2 + n), where n = number of variables)
     * word 1: reserved for Assignment 11
     * words 2 to n+1: variables
     *
     * Starting in Assignment 11, it is also necessary to know at run time which `Variable`s
     * in the run-time `Chunk` are pointers to other `Chunk`s (have their isPointer == true).
     * To enable this, word 1 of the
     * `Chunk` always holds the number of pointer `Variable`s in the chunk. In addition,
     * all of the pointer `Variable`s are allocated first, starting from word 2
     * of the `Chunk`, followed by all of the non-pointer variables. To summarize, the
     * full memory layout looks as follows:
     * word 0: size in bytes (4 * (2 + n), where n = number of variables)
     * word 1: p = number of pointer variables
     * words 2   to 2+p-1: pointer variables
     * words 2+p to 2+n-1: non-pointer variables
     */
    case class Chunk(variables: Seq[Variable]) {
        /** For Assignment 11, the variables need to be sorted as described above, so we sort them here.
         * For earlier assignments, the order of the variables is irrelevant so sorting them here doesn't affect anything.
         */
        private val sortedVariables: Seq[Variable] = {
            val (pointers, nonPointers) = variables.partition(_.isPointer)
            pointers ++ nonPointers
        }

        /** The amount of memory needed to store the chunk in words. */
        val words: Int = 2 + variables.size
        /** The amount of memory needed to store the chunk in bytes. */
        val bytes: Int = words * 4

        /** Maps each variable to the offset of its address from the start of the `Chunk`.
         *
         * Scala hint:
         *
         * Seq('a', 'b', 'c')
         *   .zipWithIndex.map{case (letter, index) => (letter, index*2)}
         *   .toMap ==
         * Map('a' -> 0, 'b' -> 2, 'c' -> 4)
         *
         */
        private val variableToOffset: Map[Variable, Int] = {
            sortedVariables
                .zipWithIndex.map{case (letter, index) => (letter, 4 * (2 + index))}
                .toMap

        }

        /** Generate code to load the value at the offset corresponding to `variable` into `register`.
         *
         * Assume that `baseRegister` contains the address of the beginning of the chunk.
         **/
        def load(baseRegister: Reg, register: Reg, variable: Variable): Code = {
            LW(register, variableToOffset(variable), baseRegister)
        }

        /** Generate code to store the value of `register` at the offset corresponding to `variable`.
         *
         * Assume that `baseRegister` contains the address of the beginning of the chunk.
         **/
        def store(baseRegister: Reg, variable: Variable, register: Reg): Code = {
            SW(register, variableToOffset(variable), baseRegister)
        }

        /** Generate code to initialize a `Chunk` that has just been allocated. The generated code should
         * assume that register `Reg.result` contains the address of the beginning of the `Chunk`.
         * It should write the size of the `Chunk` in bytes into word 0 of the `Chunk`.
         * It should set the values of all of the variables in the `Chunk` to 0.
         *
         * The generated code may modify the values of Reg.scratch. If you need more than one scratch
         * register, you may add new scratch registers to Reg.scala. The generated code must not modify the values
         * of any registers that are already listed in Reg.scala (except for Reg.scratch).
         *
         * Starting in Assignment 11, the generated code should also write the number of pointer variables into
         * word 1 of the `Chunk`.
         */
        def initialize: Code = {
            val LOOP: Label = Label("loop")
            val setSize = Seq[Code](
                LIS(Reg.scratch),
                Word(encodeUnsigned(bytes)),
                SW(Reg.scratch, 0, Reg.result)
            )
            val setNumPtrs = Seq[Code](
                LIS(Reg.scratch),
                Word(encodeUnsigned(variables.count(_.isPointer))),
                SW(Reg.scratch, 4, Reg.result)
            )
            val setZero: Seq[Code] = variables.map(variable =>
                SW(Reg.zero, variableToOffset(variable), Reg.result)
            )
            Block(setSize ++setNumPtrs++ setZero)
        }
    }

    /** An abstract memory allocator that allocates memory either on the stack or on the heap. */
    abstract class MemoryAllocator {
        /** The code to initialize the memory allocator at the beginning of the program. By default,
         * no initialization is necessary.
         */
        val initCode: Code = block()

        /** Generate the code to allocate enough space to hold `chunk` and place the address of the allocated `Chunk`
         * in `Reg.result`. This code should be followed by the code generated by `chunk.initialize`.
         */
        def allocate(chunk: Chunk): Code
    }

    /** A `MemoryAllocator` that allocates `Chunk`s of memory on the stack. */
    object Stack extends MemoryAllocator {
        /** Generate the code to allocate enough space to hold `chunk` and place the address of the allocated `Chunk`
         * in `Reg.result`. This code should be followed by the code generated by `chunk.initialize`.
         *
         * The generated code may modify the values of Reg.stackPointer, Reg.result, Reg.scratch, Reg.copyChunkScratch,
         * and Reg.scratchPtrForGC. If you need more than these registers, you may add new scratch
         * registers to Reg.scala. The generated code must not modify the values of any other registers that are
         * already listed in Reg.scala.
         */
        def allocate(chunk: Chunk): Code = block(
            Comment("pushing to Stack"),
            LIS(Reg.scratch),
            Word(encodeUnsigned(chunk.bytes)),
            SUB(Reg.stackPointer, Reg.stackPointer, Reg.scratch),
            ADD(Reg.result, Reg.stackPointer),
            chunk.initialize
        )
        /** Generate the code to deallocate the space for the `Chunk` that is at the top of the stack. To determine
         * the size of this `Chunk`, takes advantage of the convention that word 0 of each `Chunk` stores its size
         * in bytes.
         *
         * The generated code must not modify Reg.result. It may modify Reg.stackPointer and Reg.scratch.
         * If you need more than these registers, you may add new scratch registers to Reg.scala. The generated code
         * must not modify the values of any other registers that are already listed in Reg.scala.
         */
        val pop: Code = {
            block(
                Comment("popping from Stack"),
                LW(Reg.scratch, 0, Reg.stackPointer),
                ADD(Reg.stackPointer, Reg.stackPointer, Reg.scratch)
            )
        }
    }

    /** Code that copies a chunk whose address is in `fromRegister` to the address in `toRegister`.
     * `toRegister` and `fromRegister` cannot be one of the registers in `modifiedRegisters`.
     * Be careful to modify only the registers in `modifiedRegisters` in the copying code that
     * you generate. If you need to modify additional registers, add them to the `modifiedRegisters`
     * set in order to be notified if you call `copyChunk` with one of these registers as `toRegister`
     * or `fromRegister`.
     *
     * Also, do not use any Variables inside copyChunk, or any Code that depends on
     * them, that is, any code appearing after Block in ProgramRepresentation.scala,
     * particularly including whileLoop. This is because copyChunk will be used to
     * implement calls from one procedure to another, and it is not clear in which
     * procedure's frame such Variables are allocated.
     */
    def copyChunk(toRegister: Reg, fromRegister: Reg): Code = {
        /* The registers that may be modified by the code that will be generated. */
        val modifiedRegisters = Set(Reg.scratch, Reg.copyChunkScratch)
        require(!modifiedRegisters.contains(toRegister))
        require(!modifiedRegisters.contains(fromRegister))
        val startLoop = new Label("startLoop")
        val endLoop = new Label("endLoop")

        block(
            // Reg.scratch hold
            Comment("starting copying"),
            LW(Reg.scratch, 0, fromRegister),

            Define(startLoop),
            beq(Reg.scratch, Reg.zero, endLoop),


            Comment("Copy byte"),
            ADD(Reg.scratch, Reg.scratch, fromRegister),
            LW(Reg.copyChunkScratch, -4, Reg.scratch),
            SUB(Reg.scratch, Reg.scratch, fromRegister),

            ADD(Reg.scratch, Reg.scratch, toRegister),
            SW(Reg.copyChunkScratch, -4, Reg.scratch),
            SUB(Reg.scratch, Reg.scratch, toRegister),

            LIS(Reg.copyChunkScratch),
            Word(encodeSigned(4)),
            SUB(Reg.scratch, Reg.scratch, Reg.copyChunkScratch),
            beq(Reg.zero, Reg.zero, startLoop),
            Define(endLoop)
        )
    }

    private var heapImplementation: MemoryAllocator = SimpleHeapAllocator
    def heap: MemoryAllocator = heapImplementation

    trait HeapSettings {
        /** The total number of bytes of memory. */
        val memSize = decodeUnsigned(CPU.maxAddr)
        /** The address of the beginning of the heap. */
        val heapStart = Word(encodeUnsigned(memSize / 4))
        /** The address of the middle of the heap. */
        val heapMiddle = Word(encodeUnsigned(memSize / 2))
        /** The address just after the end of the heap. */
        val heapEnd = Word(encodeUnsigned(memSize * 3 / 4))

        val semiSpaceSize = Word(encodeUnsigned(memSize / 4))

    }


    /** A simple `MemoryAllocator` that allocates `Chunk`s of memory on the heap in a way that they
     * are never freed. Specifically, `Reg.heapPointer` is assumed to point to the next unused
     * memory address in the heap. To allocate a `Chunk` of a given size, the allocator just returns
     * the current value of `Reg.heapPointer` and increments it by the size so that it points to the
     * next unused word.
     */
    private object SimpleHeapAllocator extends MemoryAllocator with HeapSettings {
        /** The code to initialize the heap allocator. */
        override val initCode: Code = block(LIS(Reg.heapPointer), heapStart)

        /** Generate the code to allocate enough space to hold `chunk` and place the address of the allocated `Chunk`
         * in `Reg.result`. This code should be followed by the code generated by `chunk.initialize`.
         *
         * Note that `allocate` is called after `VarAccess` `Code`s have been eliminated. Therefore, the `Code` that
         * it returns must not contain any `VarAccess` `Code`s or `Code`s that are defined after `VarAccess` in
         * `ProgramRepresentation.scala`.
         */
        def allocate(chunk: Chunk): Code = block(
            ADD(Reg.result, Reg.heapPointer),
            LIS(Reg.scratch),
            Word(encodeSigned(chunk.bytes)),
            ADD(Reg.heapPointer, Reg.heapPointer, Reg.scratch),
            chunk.initialize,
        )
    }

    /* ## Assignment 11 */

    /** A `MemoryAllocator` that uses a copying garbage collector to reclaim `Chunk`s that are unreachable
     * from the current `Chunk` whose address is in `Reg.framePtr`. The heap is split into two halves (semispaces).
     * Memory is allocated from one of the semispaces. When the semispace becomes full, the garbage collector
     * is launched. The garbage collector copies all reachable `Chunk`s into the other semispace
     * and adjusts all pointer `Variable`s in all reachable `Chunk`s to point to the new copies. The other semispace
     * then becomes the current semispace from which memory is allocated until it becomes full, and then the whole
     * process is repeated.
     *
     * The provided `initCode` follows the assumption that `Reg.heapPointer` points to the next unused
     * word in the current semispace, and that `Reg.fromSpaceEnd` points to the word immediately
     * after the end of the current semispace.
     *
     * The first semispace starts at address heapStart and ends just before heapMiddle, and
     * the second semispace starts at address heapMiddle and ends just before heapEnd.
     */

    object GarbageCollector extends MemoryAllocator with HeapSettings {

        /** The code to initialize the heap allocator. */
        override val initCode: Code = {
            block(LIS(Reg.heapPointer), heapStart, LIS(Reg.fromSpaceEnd), heapMiddle)
        }

        /** Declarations of procedures of the memory allocator and of local variables used in those procedures.
         *
         * You may add more procedures and variables here. If you add procedures, be sure to add them to
         * `def procedures` below.
         */

        val allocateProc_bytes = new Variable("allocateProc_bytes")
        val allocateProc = new Procedure("allocateProc", Seq(allocateProc_bytes))

        val collectGarbage = new Procedure("collectGarbage", Seq())

        /** The sequence of all procedures required by memory allocator. This sequence must
         * contain `allocateProc` and `collectGarbage`, as well as any additional helper procedures that you define.
         */
        def procedures: Seq[Procedure] = Seq(allocateProc, collectGarbage, forwardPtrs, copyBlock)

        /** Code of the `allocateProc` procedure, which allocates an area of `allocateProc_bytes` consecutive bytes in memory
         * and places the address of the beginning of that area in `Reg.result`.
         *
         * If there is not enough space in the current semispace, `allocateProc` should call `collectGarbage` to try to
         * free up space. If there is still not enough space after the garbage collection pass, the behaviour of
         * `allocateProc` is undefined.
         */
        allocateProc.code = block(
            ifStmt(
                binOp(regExpr(Reg.heapPointer), plus, varExpr(allocateProc_bytes)),
                gtCmp,
                regExpr(Reg.fromSpaceEnd),
                call(collectGarbage)
            ),
            // assume heap pointer is ready
            ADD(Reg.result, Reg.heapPointer),
            read(Reg.scratch, allocateProc_bytes),
            ADD(Reg.heapPointer, Reg.heapPointer, Reg.scratch)
        )

        /** Generate the code to call the `allocateProc` procedure to allocate enough memory to hold `chunk` and place
         * the address of the allocated `Chunk` in `Reg.result`. This code should be followed by the code
         * generated by `chunk.initialize`.
         *
         * Note that `allocate` is called after `VarAccess` `Code`s have been eliminated. Therefore, the `Code` that
         * it returns must not contain any `VarAccess` `Code`s or `Code`s that are defined after `VarAccess` in
         * `ProgramRepresentation.scala`. In particular, it cannot contain the `Call` `Code`, so the code to call
         * the `allocateProc` procedure must be implemented directly in terms of simpler `Code`s.
         */
        def allocate(chunk: Chunk): Code = {
            val paramChunk = Chunk(Seq(allocateProc.staticLink, allocateProc_bytes))
            block(
                Stack.allocate(paramChunk),
                LIS(Reg.scratch),
                Word(encodeSigned(chunk.bytes)),
                paramChunk.store(Reg.result, allocateProc_bytes, Reg.scratch),
                paramChunk.store(Reg.result, allocateProc.staticLink, Reg.zero),
                LIS(Reg.targetPC),
                Use(allocateProc.label),
                JALR(Reg.targetPC),
                chunk.initialize
            )
        }

        /** The code of the procedure that performs a garbage collection pass over the heap. The procedure should take
         * zero parameters, and it should return in Reg.result an `Int`, the total number of bytes of memory
         * in the heap (that is, the sum of the sizes in bytes of all of the live objects remaining in the heap
         * after the garbage collection pass).
         *
         * You may assume that the `collectGarbage` procedure will only ever be called from a procedure whose frame
         * is stored on the stack, not on the heap. Thus, you may assume that the dynamic link in the `collectGarbage`
         * procedure points to what was the top of the stack before the `collectGarbage` procedure was called.
         */


        // returns address of copied block

        val toSpaceStart = new Variable("toStart")
        val free = new Variable ("free")
        val scan = new Variable("scan")

        collectGarbage.code = Scope(Seq(toSpaceStart, scan, free), block(
            assign(toSpaceStart, ifStmt(regExpr(Reg.fromSpaceEnd), eqCmp, block(LIS(Reg.result), heapMiddle), // heapPointer in left half
                block(LIS(Reg.result), heapMiddle),
                block(LIS(Reg.result), heapStart)
            )),
            assign(free, varExpr(toSpaceStart)),
            assign(scan, varExpr(collectGarbage.dynamicLink)),

            whileLoop(varExpr(scan), ltCmp, block(LIS(Reg.result), CPU.maxAddr), block(
                call(forwardPtrs, varExpr(scan)), // forwardPtrs handles setting updating ptrs in block
                assign(scan, binOp(varExpr(scan), plus, deref(varExpr(scan)))),
            )),

            assign(scan, varExpr(toSpaceStart)),
            whileLoop(varExpr(scan), ltCmp, varExpr(free), block(
                call(forwardPtrs, varExpr(scan)),
                assign(scan, binOp(varExpr(scan), plus, deref(varExpr(scan)))),
            )),
            binOp(varExpr(toSpaceStart), plus, block(LIS(Reg.result), semiSpaceSize)),
            ADD(Reg.fromSpaceEnd, Reg.result),
            read(Reg.heapPointer, free),
        ))

        lazy val forwardPtrs: Procedure = {
            val chunkAddr = new Variable("chunkAddr")
            val procedure = new Procedure("forwardPtrs", Seq(chunkAddr), outer=Some(collectGarbage))

            val chunkPtrIdx = new Variable("chunkPtrIdx")
            val chunkNumPtrs = new Variable("chunkNumPtrs")
            val chunkPtrLoc = new Variable("chunkPtrLoc")
            val newAddr = new Variable("newAddr")

            procedure.code = Scope(Seq(chunkPtrIdx, chunkNumPtrs, chunkPtrLoc, newAddr), block(
                assign(chunkNumPtrs, deref(binOp(varExpr(chunkAddr), plus, literal(4)))),
                assign(chunkPtrIdx, literal(0)),
                whileLoop(varExpr(chunkPtrIdx), ltCmp, varExpr(chunkNumPtrs), block(
                    assign(chunkPtrLoc, binOp(
                        binOp(varExpr(chunkAddr), plus, literal(8)),
                        plus,
                        binOp(varExpr(chunkPtrIdx), times, literal(4)),
                    )),
                    assign(newAddr, call(copyBlock, deref(varExpr(chunkPtrLoc)))),
                    assignToAddr(varExpr(chunkPtrLoc), varExpr(newAddr)),
                    assign(chunkPtrIdx, binOp(varExpr(chunkPtrIdx), plus, literal(1))),
                ))
            ))
            procedure
        }

        lazy val copyBlock: Procedure = {
            val fromAddr = new Variable("fromAddr")

            val procedure = new Procedure("copyBlock", Seq(fromAddr), outer=Some(collectGarbage))
            val blockSize = new Variable("blockSize")
            procedure.code = Scope(Seq(blockSize), block(
                // get fromStart and fromEnd
                assign(blockSize, deref(varExpr(fromAddr))), // blockSize = *blockAddr
                ifStmt(varExpr(fromAddr), geCmp,
                    binOp(regExpr(Reg.fromSpaceEnd), minus, block(LIS(Reg.result), semiSpaceSize)),
                    ifStmt(varExpr(fromAddr), ltCmp, regExpr(Reg.fromSpaceEnd),
                        block(// if block is in heap
                            ifStmt(varExpr(blockSize), geCmp, literal(0), block(
                                // assume heap pointer is in free space
                                read(Reg.scratchPtrForGC, fromAddr),
                                read(Reg.result, free),
                                copyChunk(Reg.result, Reg.scratchPtrForGC),
                                assignToAddr(binOp(varExpr(fromAddr), plus, literal(4)), varExpr(free)),
                                assignToAddr(varExpr(fromAddr), binOp(literal(0), minus, varExpr(blockSize))),
                                assign(free, binOp(varExpr(free), plus, deref(varExpr(free))))
                            )),
                            deref(binOp(varExpr(fromAddr), plus, literal(4)))
                        ),
                        varExpr(fromAddr)
                    ),
                    varExpr(fromAddr)
                ),
            ))
            procedure
        }
    }

    /** Runs the provided Scala code block using the GarbageCollector implementation of the heap. */
    def withGC[T](code: =>T): T = withHeap(GarbageCollector, code)

    def withHeap[T](newHeap: MemoryAllocator, code: =>T): T = try {
        heapImplementation = newHeap
        code
    } finally { heapImplementation = SimpleHeapAllocator }
}
