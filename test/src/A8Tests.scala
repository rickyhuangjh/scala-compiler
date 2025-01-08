import cs241e.assignments.*
import cs241e.assignments.Assembler.*
import cs241e.assignments.CodeBuilders.*
import cs241e.assignments.MemoryManagement.*
import cs241e.assignments.Parsing.*
import cs241e.assignments.ProgramRepresentation.*
import cs241e.assignments.Transformations.*
import cs241e.mips.*
import cs241e.nosource.ParsingPrivate
import cs241e.scanparse.DFAs.Token
import cs241e.scanparse.Grammars
import org.scalatest.funsuite.AnyFunSuite

class A8TestsL1 extends AnyFunSuite {
    test("simple-grammar") {
        val grammar = Grammars.parseGrammar(
            """
                S BOF expr EOF
                expr ID
                expr expr op expr
                op +
                op *
            """
        )
        println(grammar.nonTerminals)
        println(grammar.terminals)
        println(grammar.symbols)
        println(grammar.start)
        println(grammar.productions)
        println(grammar.productionsExpanding("op"))

        val tokens = Seq(Token("BOF"), Token("ID"), Token("+"),
            Token("ID"), Token("*"), Token("ID"), Token("EOF"))
        val tree = parseCYK(grammar, tokens.toIndexedSeq).get
        // println(tree.production)
        // println(tree.children(1).production)
        println(tree)

        val badtokens = Seq(Token("ID"), Token("+"), Token("ID"), Token("*"))
        assert(parseCYK(grammar, badtokens.toIndexedSeq) == None)
    }
    test("lacs") {
        //    println(Lacs.grammar)
        val prog =
            """
      def main(a: Int, b: Int): Int = {a * b + 1}
      def main(a: Int, b: Int): Int = {a * b + 1}

        """
        val tokens = Lacs.scan(prog)
        // println(tokens.map(x => x.kind))
        println(Lacs.grammar.terminals)
        val tree = parseCYK(Lacs.grammar, tokens.toIndexedSeq).get
        // println(tree)
        val tree2 = Lacs.scanAndParse(prog)
        println(tree2)
    }
}