import scala.util.control.Breaks._

/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Prg 01 - SyntaxAnalyzer (an iterable syntax analyzer)
 * Student(s) Name(s): Cameron Jensen
 */

class SyntaxAnalyzer(private var source: String) {

  private val it = new LexicalAnalyzer(source).iterator
  private var current: Lexeme = null

  // returns the current lexeme
  private def getLexeme: Lexeme = {
    if (current == null)
      nextLexeme
    current
  }

  // advances the input one lexeme
  private def nextLexeme = {
    current = it.next
  }

  // returns true if the given token identifies a statement (or the beginning of a statement)
  private def isStatement(token: Token.Value): Boolean = {
    token == Token.IDENTIFIER ||
      token == Token.LITERAL ||
      token == Token.STRING ||
      token == Token.INPUT ||
      token == Token.OUTPUT ||
      token == Token.ASSIGNMENT ||
      token == Token.ADDITION ||
      token == Token.SUBTRACTION ||
      token == Token.MULTIPLICATION ||
      token == Token.DIVISION ||
      token == Token.MODULUS ||
      token == Token.LESS ||
      token == Token.LESS_EQUAL ||
      token == Token.GREATER ||
      token == Token.GREATER_EQUAL ||
      token == Token.EQUAL ||
      token == Token.DIFFERENT ||
      token == Token.BREAK ||
      token == Token.DOT ||
      token == Token.OPEN_BRACKET ||
      token == Token.OPEN_PAR
  }

  // returns true if the given token identifies a line (or the beginning of a line)
  // a line can be a statement or a comma -- comment?
  private def isLine(token: Token.Value): Boolean = {
    isStatement(token) || token == Token.COMMENT
  }

  // parses the program, returning its corresponding parse tree
  def parse: Node = {
    parseMouse
  }

  private def parseMouse: Node = {
    val node = new Node(new Lexeme("mouse"))
    while (it.hasNext) {
      nextLexeme
      var lexeme = getLexeme
      if (isLine(lexeme.token))
        node.add(parseLine)
      else if (lexeme.token.equals(Token.EO_PRG)) {
        node.add(new Node(lexeme))
      }
      else throw new Exception("Syntax Error: expected Lexeme($$, EO_PRG)")
    }
    node
  }

  private def parseLine: Node = {
    val node = new Node(new Lexeme("line"))
    var lexeme = getLexeme
    if (isStatement(lexeme.token)) node.add(parseStatement)
    else node.add(new Node(getLexeme))
    node
  }

  private def parseStatement: Node = {
    val node = new Node(new Lexeme("statement"))
    var lexeme = getLexeme
    if (lexeme.token.equals(Token.OPEN_BRACKET)) node.add(parseIf)
    else if (lexeme.token.equals(Token.OPEN_PAR)) node.add(parseWhile)
    else node.add(new Node(lexeme))
    node
  }

  def parseIf: Node = {
    val node = new Node(new Lexeme("if"))
    var lexeme: Lexeme = null
    breakable {
      while (it.hasNext) {
        nextLexeme
        lexeme = getLexeme
        if (isLine(lexeme.token)) node.add(parseLine)
        else if (lexeme.token.equals(Token.CLOSE_BRACKET)) {
          node.add(new Node(lexeme))
          break
        }
        else throw new Exception("Syntax Error: expected line or ']'")
      }
    }
    node
  }

  def parseWhile: Node = {
    val node = new Node(new Lexeme("while"))
    var lexeme: Lexeme = null
    breakable {
      while (it.hasNext) {
        nextLexeme
        lexeme = getLexeme
        if (isLine(lexeme.token)) node.add(parseLine)
        else if (lexeme.token.equals(Token.CLOSE_PAR)) {
          node.add(new Node(lexeme))
          break
        }
        else throw new Exception("Syntax Error: expected line or ')'")
      }
    }
    node
  }
}

object SyntaxAnalyzer {
  def main(args: Array[String]): Unit = {

    // check if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    val syntaxAnalyzer = new SyntaxAnalyzer(args(0))
    val parseTree = syntaxAnalyzer.parse
    print(parseTree)
  }
}
