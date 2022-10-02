/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Prg 01 - LexicalAnalyzer (an iterable lexical analyzer)
 * Student(s) Name(s): Cameron Jensen
 */

import LexicalAnalyzer.{BLANKS, DIGITS, LETTERS, NEW_LINE, PUNCTUATIONS, SPECIALS}
import scala.io.{BufferedSource, Source}

class LexicalAnalyzer(private var source: String) extends Iterable[Lexeme] {
  var input = ""
  val fBufSource: BufferedSource = Source.fromFile(source)
  for (line <- fBufSource.getLines)
    input += line + LexicalAnalyzer.NEW_LINE
  fBufSource.close()
  input = input.trim

  // Checks if reached eof
  private def eof: Boolean = input.isEmpty

  var currentChar: Char = 0

  // Returns the current char
  private def getChar = {
    if (!eof)
      currentChar = input(0)
    currentChar
  }

  // Advances the input one character
  private def nextChar: Unit = {
    if (!eof)
      input = input.substring(1)
  }

  // Checks if input has a blank character ahead
  private def hasBlank: Boolean = {
    LexicalAnalyzer.BLANKS.contains(getChar)
  }

  // Reads the input until a non-blank character is found, updating the input
  def readBlanks: Unit = {
    while (!eof && hasBlank)
      nextChar
  }

  // Checks if input has a letter ahead
  private def hasLetter: Boolean = {
    LexicalAnalyzer.LETTERS.contains(getChar)
  }

  // Checks if input has a digit ahead
  private def hasDigit: Boolean = {
    LexicalAnalyzer.DIGITS.contains(getChar)
  }

  // Checks if input has a special character ahead
  private def hasSpecial: Boolean = {
    LexicalAnalyzer.SPECIALS.contains(getChar)
  }

  // Checks if input has a punctuation character ahead
  private def hasPunctuation: Boolean = {
    LexicalAnalyzer.PUNCTUATIONS.contains(getChar)
  }

  // Returns an iterator for the lexical analyzer
  override def iterator: Iterator[Lexeme] = {
    new Iterator[Lexeme] {
      // Returns true/false depending whether there is a lexeme to be read from the input
      override def hasNext: Boolean = {
        readBlanks
        !eof
      }

      override def next(): Lexeme = {
        var symbol: String = ""
        var token: Token.Value = null
        lazy val invalidSymbolString =
          "Lexical Analyzer Error: unrecognizable symbol (multi-char) \"" + symbol + "\" found!"
        lazy val invalidSymbolChar =
          "Lexical Analyzer Error: unrecognizable symbol \"" + getChar + "\" found!"

        readBlanks
        if (!hasNext)
          return new Lexeme("eof", Token.EOF)

        if (hasLetter) {
          lazy val hasIdentifierChar =
            hasLetter || hasDigit || getChar.equals('_')
          do {
            symbol += getChar
            nextChar
          } while (!eof && hasIdentifierChar)
          token = Token.IDENTIFIER
          if (getChar.equals('.')) return new Lexeme(symbol, token)
        }
        else if (hasDigit) {
          do {
            symbol += getChar
            nextChar
          } while (hasDigit)
          token = Token.LITERAL
        }
        else if (hasPunctuation) {
          if (getChar.equals(';')) {
            do {
              symbol += getChar
              nextChar
            } while (!NEW_LINE.contains(getChar))
            token = Token.COMMENT
          }
          else if (getChar.equals('?')) {
            symbol += getChar
            nextChar
            if (BLANKS.contains(getChar)) token = Token.INPUT
            else throw new Exception(invalidSymbolString)
          }
          else if (getChar.equals('!')) {
            symbol += getChar
            nextChar
            if (getChar.equals('=')) token = Token.DIFFERENT
            else if (BLANKS.contains(getChar)) token = Token.OUTPUT
            else throw new Exception(invalidSymbolString)
          }
          else if (getChar.equals('.')) {
            symbol += getChar
            token = Token.DOT
          }
        }
        else if (hasSpecial) {
          symbol += getChar
          if (getChar.equals('=')) {
            nextChar
            if (getChar.equals('=')) {
              symbol += getChar
              token = Token.EQUAL
            }
            else token = Token.ASSIGNMENT
          }
          else if (getChar.equals('<')) {
            nextChar
            symbol += getChar
            if (getChar.equals('=')) token = Token.LESS_EQUAL
            else if (BLANKS.contains(getChar)) token = Token.LESS
          }
          else if (getChar.equals('>')) {
            nextChar
            if (getChar.equals('=')) {
              symbol += getChar
              token = Token.GREATER_EQUAL
            }
            else token = Token.GREATER
          }
          else if (getChar.equals('"')) {
            nextChar
            while (!getChar.equals('"')) {
              symbol += getChar
              nextChar
            }
            symbol += getChar
            token = Token.STRING
          }
          else if (getChar.equals('+')) token = Token.ADDITION
          else if (getChar.equals('-')) token = Token.SUBTRACTION
          else if (getChar.equals('*')) token = Token.MULTIPLICATION
          else if (getChar.equals('/')) token = Token.DIVISION
          else if (getChar.equals('%')) token = Token.MODULUS

          else if (getChar.equals('(')) token = Token.OPEN_PAR
          else if (getChar.equals(')')) token = Token.CLOSE_PAR
          else if (getChar.equals('[')) token = Token.OPEN_BRACKET
          else if (getChar.equals(']')) token = Token.CLOSE_BRACKET

          else if (getChar.equals('^')) token = Token.BREAK
          else if (getChar.equals('$')) {
            nextChar
            symbol += getChar
            if (getChar.equals('$')) token = Token.EO_PRG
          }
        }

        // Throw exception if an unrecognizable symbol is found
        if (token == null) {
          println(getChar)
          throw new Exception(invalidSymbolChar)
        }
        else {
          nextChar
          new Lexeme(symbol, token)
        }
      }
    }
  }
}

object LexicalAnalyzer {
  val BLANKS = " \n\t"
  val NEW_LINE = "\n"
  val LETTERS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val DIGITS = "0123456789"
  val PUNCTUATIONS = ".,;:?!"
  val SPECIALS = "><_*@#$%^&()-+='\"/\\[]{}|"

  def main(args: Array[String]): Unit = {

    // Checks the command - line for source file
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    // Iterates over the lexical analyzer, printing the lexemes found
    val lex = new LexicalAnalyzer(args(0))
    val it = lex.iterator
    while (it.hasNext)
      println(it.next())

  } // End main method
}
