import cs241e.assignments.A1
import cs241e.assignments.A2
import cs241e.assignments.Assembler.*
import cs241e.mips.*
import org.scalatest.funsuite.AnyFunSuite

/* This is an example of a class for running and testing your code. Add other testing methods here
 * and/or create additional classes like this one.
 *
 * Run the tests by right-clicking on the test code and selecting Run '(test name)' from the menu.
 *
 * You can also run the tests by right-clicking on a file or directory containing tests in the Project navigator
 * on the left.
 */

class A1Tests extends AnyFunSuite {
  def bitsToInt(bits: Seq[Boolean]): Int = {
    bits.foldLeft(0) { (acc, bit) =>
      (acc << 1) | (if (bit) 1 else 0)
    }
  }
  test("decodeUnsigned") {

    println("You can print output from your tests.")


    assert(1 + 1 == 2, "1 + 1 did not equal 2.")

    // The following will fail until you implement decodeUnsigned as part of Assignment 1.
    println(decodeUnsigned(Seq(false)))
    println(decodeSigned(Seq(true, false, true, true, false)))
    val res = encodeSigned(-1154715079, 32)
    println(res)
    println(res.length)
    print(bitsToInt(res))
  }

  test("program") {
    println(decodeSigned(encodeSigned(Int.MinValue)))
  }
}
