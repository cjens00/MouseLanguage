/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Prg 01 - LexicalAnalyzer (an iterable lexical analyzer)
 * Student(s) Name(s): Cameron Jensen
 */

import LexicalAnalyzer.{BLANKS, DIGITS, LETTERS, NEW_LINE, PUNCTUATIONS, SPECIALS}

import java.io.File
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
        if (!hasNext)
          return new Lexeme("eof", Token.EOF)

        var shouldReturnLexeme: Boolean = false
        var lexemeString: String = ""
        var token: Token.Value = null
        lazy val invalidLexString =
          "Lexical Analyzer Error: unrecognizable symbols \"" + lexemeString + "\" found!"
        lazy val invalidLexChar =
          "Lexical Analyzer Error: unrecognizable symbol \"" + getChar + "\" found!"

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
              if (getChar.equals('?')) {
                nextChar
                lexemeString += getChar
                if (BLANKS.contains(getChar)) token = Token.INPUT
                else throw new Exception(invalidLexString)
              }
              if (getChar.equals('!')) {
                nextChar
                lexemeString += getChar
                if (getChar.equals('=')) token = Token.DIFFERENT
                else if (BLANKS.contains(getChar)) token = Token.OUTPUT
                else throw new Exception(invalidLexString)
              }

            }
            shouldReturnLexeme = true
          }
          else if (hasSpecial) {
            lexemeString += getChar
            if (getChar.equals('=')) {
              nextChar
              lexemeString += getChar
              if (getChar.equals('=')) token = Token.EQUAL
              else if (BLANKS.contains(getChar)) token = Token.ASSIGNMENT
              else throw new Exception(invalidLexString)
            }
            else if (getChar.equals('<')) {
              nextChar
              lexemeString += getChar
              if (getChar.equals('=')) token = Token.LESS_EQUAL
              else if (BLANKS.contains(getChar)) token = Token.LESS
              else throw new Exception(invalidLexString)
            }
            else if (getChar.equals('>')) {
              nextChar
              lexemeString += getChar
              if (getChar.equals('=')) token = Token.GREATER_EQUAL
              else if (BLANKS.contains(getChar)) token = Token.GREATER
              else throw new Exception(invalidLexString)
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
            shouldReturnLexeme = true
          }
        }
        // Throw exception if an unrecognizable symbol is found
        if (token == null) throw new Exception(invalidLexChar)
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

    val checkDir = {
      val dir: File = new File(".")
      dir.listFiles.filter(_.isFile).toList
    }
    // println(checkDir)

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
