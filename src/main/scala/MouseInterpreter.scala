/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Prg 01 - MouseInterpreter
 * Student(s) Name(s): Cameron Jensen
 */

import scala.collection.mutable
import scala.io.StdIn
import scala.util.control.Breaks.{break, breakable}

class MouseInterpreter(private var parseTree: Node) {
  // Set this to true to get detailed debug info
  final val DEBUG = true
  private val stack = mutable.Stack[Int]()
  private val memory = mutable.LinkedHashMap[String, Int]()
  private val InterpretString = (lexeme: Lexeme) => {
    print(lexeme.label)
  }
  private val InterpretIdentifier = (lexeme: Lexeme) => {
    stack.push(addressOf(lexeme.label))
  }
  private val InterpretLiteral = (lexeme: Lexeme) => {
    stack.push(lexeme.label.toInt)
  }
  private val InterpretInput = () => {
    stack.push(StdIn.readInt())
  }
  private val InterpretOutput = () => {
    print(stack.pop)
  }
  private val InterpretAssignment = () => {
    val b = stack.pop
    val a = stack.pop
    memory(getNameOf(a)) = b
  }
  private val InterpretAddition = () => {
    val b = stack.pop
    val a = stack.pop
    stack.push(a + b)
  }
  private val InterpretSubtraction = () => {
    val b = stack.pop
    val a = stack.pop
    stack.push(a - b)
  }
  private val InterpretMultiplication = () => {
    val b = stack.pop
    val a = stack.pop
    stack.push(a * b)
  }
  private val InterpretDivision = () => {
    val b = stack.pop
    val a = stack.pop
    stack.push(a / b)
  }
  private val InterpretModulus = () => {
    val b = stack.pop
    val a = stack.pop
    stack.push(a % b)
  }
  private val InterpretLessThan = () => {
    val b = stack.pop
    val a = stack.pop
    stack.push(if (a < b) 1 else 0)
  }
  private val InterpretLessThanEqualTo = () => {
    val b = stack.pop
    val a = stack.pop
    stack.push(if (a <= b) 1 else 0)
  }
  private val InterpretGreaterThan = () => {
    val b = stack.pop
    val a = stack.pop
    stack.push(if (a > b) 1 else 0)
  }
  private val InterpretGreaterThanEqualTo = () => {
    val b = stack.pop
    val a = stack.pop
    stack.push(if (a >= b) 1 else 0)
  }
  private val InterpretEqualTo = () => {
    val b = stack.pop
    val a = stack.pop
    stack.push(if (a == b) 1 else 0)
  }
  private val InterpretNotEqualTo = () => {
    val b = stack.pop
    val a = stack.pop
    stack.push(if (a != b) 1 else 0)
  }
  private val InterpretDot = () => {
    val address = stack.pop
    val name = getNameOf(address)
    stack.push(valueOf(name))
  }
  private val InterpretIf = (branch: Node) => {
    val execLine = branch.getBranches().iterator
    execLine.next // Consumes open bracket
    var done = false
    if (stack.pop() != 0) {
      while (!done) {
        val line = execLine.next()
        var lexeme = line.lexeme
        if (lexeme.token == Token.CLOSE_BRACKET)
          done = true
        else if (lexeme.label.equals("line")) {
          val stmtOrComment = line.getBranches()(0)
          lexeme = stmtOrComment.lexeme
          if (lexeme.label.equals("statement"))
            run(stmtOrComment)
        }
        else
          throw new Error("Runtime Error: line expected!")
      }
    }
  }
  private val InterpretWhile = (branch: Node) => {
    var loopDone = false
    while (!loopDone) {
      val execLine = branch.getBranches().iterator
      execLine.next // Consumes open parenthesis
      var iterationDone = false
      while (!iterationDone) {
        val line = execLine.next()
        var lexeme = line.lexeme
        if (lexeme.token.equals(Token.CLOSE_PAR))
          iterationDone = true
        else if (lexeme.label.equals("line")) {
          val stmtOrComment = line.getBranches()(0)
          lexeme = stmtOrComment.lexeme
          if (lexeme.label.equals("statement")) {
            val stmt = stmtOrComment.getBranches()(0)
            lexeme = stmt.lexeme
            if (lexeme.token.equals(Token.BREAK) && stack.pop() != 0) {
              iterationDone = true
              loopDone = true
            }
            else run(stmtOrComment)
          }
        }
      }
    }
  }

  // Displays the state of the stack and the memory for debug purposes
  def displayStackAndMemory = {
    print("Stack: [top] ")
    for (el <- stack) print(el + " ")
    println("[bottom]")
    println("Memory: ")
    var address = 0
    for (tuple <- memory) {
      println(s"\t${address}: ${tuple._1}=${tuple._2}")
      address += 1
    }
  }

  // Returns the "address" of a given variable (name)
  def addressOf(name: String): Int = {
    if (!memory.contains(name)) memory(name) = 0
    var address = 0
    for (tuple <- memory) {
      if (tuple._1.equals(name)) return address
      address += 1
    }
    throw new Error("Runtime Error: memory corrupted!")
  }

  // Returns the value of a given variable (name)
  def valueOf(name: String): Int = {
    if (!memory.contains(name)) memory(name) = 0
    memory(name)
  }

  // returns the (name) of a variable given its "address"
  def getNameOf(address: Int): String = {
    var currentAddress = 0
    for (tuple <- memory) {
      if (currentAddress == address)
        return tuple._1
      currentAddress += 1
    }
    throw new Error("Runtime Error: address not found!")
  }

  // Runs the main execution line until end-of-program is reached
  def run(): Unit = {
    val execLine = parseTree.getBranches().iterator
    breakable {
      while (true) {
        val line = execLine.next()
        var lexeme = line.lexeme
        if (lexeme.token == Token.EO_PRG)
          break
        else if (lexeme.label.equals("line")) {
          val stmtOrComment = line.getBranches()(0)
          lexeme = stmtOrComment.lexeme
          if (lexeme.label.equals("statement")) run(stmtOrComment)
        }
        else
          throw new Error("Runtime Error: line expected!")
      }
    }
  }

  // Runs a single (given) statement
  def run(stmt: Node): Unit = {
    val branch = stmt.getBranches()(0)
    val lexeme = branch.lexeme
    if (DEBUG) {
      displayStackAndMemory
      println(s"Statement: ${lexeme}\n")
    }
    lexeme.token match {
      case Token.STRING => InterpretString(lexeme)
      case Token.IDENTIFIER => InterpretIdentifier(lexeme)
      case Token.LITERAL => InterpretLiteral(lexeme)
      case Token.INPUT => InterpretInput()
      case Token.OUTPUT => InterpretOutput()
      case Token.ASSIGNMENT => InterpretAssignment()
      case Token.ADDITION => InterpretAddition()
      case Token.SUBTRACTION => InterpretSubtraction()
      case Token.MULTIPLICATION => InterpretMultiplication()
      case Token.DIVISION => InterpretDivision()
      case Token.MODULUS => InterpretModulus()
      case Token.LESS => InterpretLessThan()
      case Token.LESS_EQUAL => InterpretLessThanEqualTo()
      case Token.GREATER => InterpretGreaterThan()
      case Token.GREATER_EQUAL => InterpretGreaterThanEqualTo()
      case Token.EQUAL => InterpretEqualTo()
      case Token.DIFFERENT => InterpretNotEqualTo()
      case Token.DOT => InterpretDot()
      case _ =>
        lexeme.label match {
          case "if" => InterpretIf(branch)
          case "while" => InterpretWhile(branch)
          case _ => throw new Error("Runtime Error: Invalid token.")
        }
    }
  }
}

object MouseInterpreter {
  def main(args: Array[String]): Unit = {
    // Check command line arguments for an input file
    if (args.length != 1) {
      println("No input file was provided.")
      System.exit(1)
    }
    val syntaxAnalyzer = new SyntaxAnalyzer(args(0))
    val parseTree = syntaxAnalyzer.parse
    val interpreter = new MouseInterpreter(parseTree)
    interpreter.run()
  }
}
