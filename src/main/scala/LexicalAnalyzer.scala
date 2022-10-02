/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Prg 01 - LexicalAnalyzer (an iterable lexical analyzer)
 * Student(s) Name(s): Cameron Jensen
 */

import LexicalAnalyzer.{BLANKS, DIGITS, LETTERS, NEW_LINE, PUNCTUATIONS, SPECIALS}
import scala.io.Source

class LexicalAnalyzer(private var source: String) extends Iterable[Lexeme] {

  var input = ""
  for (line <- Source.fromFile(source).getLines)
    input += line + LexicalAnalyzer.NEW_LINE
  input = input.trim

  // checks if reached eof
  private def eof: Boolean = input.length == 0

  var currentChar: Char = 0

  // returns the current char
  private def getChar = {
    if (!eof)
      currentChar = input(0)
    currentChar
  }

  // advances the input one character
  private def nextChar: Unit = {
    if (!eof)
      input = input.substring(1)
  }

  // checks if input has a blank character ahead
  private def hasBlank: Boolean = {
    LexicalAnalyzer.BLANKS.contains(getChar)
  }

  // reads the input until a non-blank character is found, updating the input
  def readBlanks: Unit = {
    while (!eof && hasBlank)
      nextChar
  }

  // checks if input has a letter ahead
  private def hasLetter: Boolean = {
    LexicalAnalyzer.LETTERS.contains(getChar)
  }

  // checks if input has a digit ahead
  private def hasDigit: Boolean = {
    LexicalAnalyzer.DIGITS.contains(getChar)
  }

  // checks if input has a special character ahead
  private def hasSpecial: Boolean = {
    LexicalAnalyzer.SPECIALS.contains(getChar)
  }

  // checks if input has a punctuation character ahead
  private def hasPunctuation: Boolean = {
    LexicalAnalyzer.PUNCTUATIONS.contains(getChar)
  }

  // returns an iterator for the lexical analyzer
  override def iterator: Iterator[Lexeme] = {

    new Iterator[Lexeme] {

      // returns true/false depending whether there is a lexeme to be read from the input
      override def hasNext: Boolean = {
        readBlanks
        !eof
      }

      // TODO: return the next lexeme (or Token.EOF if there isn't any lexeme left to be read)
      override def next(): Lexeme = {

        if (!hasNext)
          return new Lexeme("eof", Token.EOF)

        // =====================================
        var shouldReturnLexeme: Boolean = false
        var lexemeString: String = ""
        var token: Token.Value = null

        while (!shouldReturnLexeme) {
          if (hasLetter) {
            val hasIdentifierChar = hasLetter || hasDigit || getChar.equals('_')
            do {
              lexemeString += getChar
              nextChar
            } while (!eof && hasIdentifierChar)
            token = Token.IDENTIFIER
            shouldReturnLexeme = true
          }
          else if (hasDigit) {
            do {
              lexemeString += getChar
              nextChar
            } while (hasDigit)
            token = Token.LITERAL
            shouldReturnLexeme = true
          }
          else if (hasPunctuation) {
            if (getChar.equals(';')) {
              do {
                lexemeString += getChar
                nextChar
              } while (!NEW_LINE.contains(getChar))
              // Consume again for windows CRLF
              if (getChar == '\r') nextChar
              token = Token.COMMENT
            }
            else {
              // --------------
              // Not a comment
              // --------------
            }
            shouldReturnLexeme = true
          }
          else if (hasSpecial) {
            lexemeString += getChar
            if (getChar.equals('=')) token = Token.ASSIGNMENT
            if (getChar.equals('!')) {
              nextChar
              lexemeString += getChar
              if (getChar.equals('=')) {
                token = Token.EQUAL
              }
              else if (BLANKS.contains(getChar)) token = Token.OUTPUT
            }
            if (getChar.equals('<')) {
              nextChar
              lexemeString += getChar
              if (getChar.equals('=')) token = Token.LESS_EQUAL
              else if (BLANKS.contains(getChar)) token = Token.LESS
            }
            if (getChar.equals('>')) {
              nextChar
              lexemeString += getChar
              if (getChar.equals('=')) token = Token.GREATER_EQUAL
              else if (BLANKS.contains(getChar)) token = Token.GREATER
            }
            if (getChar.equals('?'))    token = Token.INPUT
            if (getChar.equals('+'))    token = Token.ADDITION
            if (getChar.equals('-'))    token = Token.SUBTRACTION
            if (getChar.equals('*'))    token = Token.MULTIPLICATION
            if (getChar.equals('/'))    token = Token.DIVISION
            if (getChar.equals('%'))    token = Token.MODULUS

            if (getChar.equals('('))    token = Token.OPEN_PAR
            if (getChar.equals(')'))    token = Token.CLOSE_PAR
            if (getChar.equals('['))    token = Token.OPEN_BRACKET
            if (getChar.equals(']'))    token = Token.CLOSE_BRACKET

            shouldReturnLexeme = true
          }
        }
        // Throw exception if an unrecognizable symbol is found
        if (token == null) throw new Exception(
          "Lexical Analyzer Error: unrecognizable symbol \"" + getChar + "\" found!")
        else {
          nextChar
          new Lexeme(lexemeString, token)
        }
      }
    }
  }
}

object LexicalAnalyzer {
  val BLANKS = " \n\t"
  val NEW_LINE = "\r\n"
  val LETTERS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val DIGITS = "0123456789"
  val PUNCTUATIONS = ".,;:?!"
  val SPECIALS = "<_@#$%^&()-+='\"/\\[]{}|"

  def main(args: Array[String]): Unit = {

    // checks the command-line for source file
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    // iterates over the lexical analyzer, printing the lexemes found
    val lex = new LexicalAnalyzer(args(0))
    val it = lex.iterator
    while (it.hasNext)
      println(it.next())

  } // end main method
}
