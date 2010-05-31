package com.blogspot.speakingmylanguage.lightmark

import _root_.scala.util.parsing.combinator.{Parsers, ImplicitConversions}
import _root_.scala.util.parsing.input._
import _root_.scala.xml.{Elem => XmlElem, MetaData, NodeSeq, Null, Text, TopScope, Unparsed, UnprefixedAttribute, Group, Node}
import _root_.scala.collection.mutable.HashMap

import java.util.regex.Pattern

import scala.collection.immutable.Stack

class reStructuredTextParser {
  import reStructuredTextParser._

  // A structure indicating which character underlines each section level
  var headingLevels: Stack[Char] = Stack.Empty

  def getLevel(c: Char): Int = {
    if (!(headingLevels contains c))
      headingLevels = headingLevels push c
    return (headingLevels findIndexOf { c == _ }) + 1
  }

  lazy val title: Parser[reStructuredText] = overlineTitle | underlineTitle

  lazy val overlineTitle: Parser[reStructuredText] = separator ~ newline ~ rep1(not(newline) ~ whiteSpace ~> anyChar) ~ newline ~ separator <~ whiteSpace ^? {
    case overline ~ _ ~ text ~ _ ~ underline
      if underline.length >= text.length &&
         underline.length == overline.length &&
         overline.c == underline.c => {
           Section(text.mkString, getLevel(underline.c))
         }
  }

  lazy val underlineTitle: Parser[reStructuredText] = not(wsc) ~> rep1(not(newline) ~> anyChar) ~ newline ~ separator <~ whiteSpace ^? {
    case text ~ _ ~ underline
      if underline.length >= text.length =>
        Section(text.mkString, getLevel(underline.c))
  }

  lazy val rst = (title | block(0))*
}

object reStructuredTextParser extends Parsers with ImplicitConversions {

  implicit def strToInput(in: String): Input = new CharArrayReader(in.toCharArray)
  /*implicit def str2chars(s: String): List[Char] = augmentString(s).toList*/

  type Elem = Char

  val punctRegex = Pattern.compile("""\p{Punct}""")

  def isPunct(c: Char) = punctRegex.matcher(c.toString).matches

  lazy val punct: Parser[Elem] = elem("separator", isPunct)

  lazy val separator = repFirst(punct) ^^ {
    case s => Separator(s.head, s.length)
  }

  // A parser which captures the first element and matches repetitions
  // of the same character
  def repFirst(p: Parser[Elem]): Parser[List[Elem]] = Parser { in =>
    p(in) match {
      case s @ Success(v, _) => rep(v)(in)
      case e @ Error(msg, _) => Error(msg, in)
      case f @ Failure(msg, _) => Failure(msg, in)
    }
  }

  // elem('!') | '"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@' | '[' | '\\' | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~'

  lazy val bullet: Parser[Elem] = elem('*') | '+' | '-' | '•' | '‣' | '⁃'

  lazy val letter: Parser[Elem] = elem("Letter", c => Character.isLetter(c))

  lazy val newline = accept("\r\n".toList) | '\n' | '\r'

  def wsc(c: Char): Boolean = Character.isWhitespace(c) // ' ' || c == '\n' || c == '\r' || c == '\t'
  def wsc: Parser[Elem] = elem("wsc", wsc)

  lazy val space: Parser[Elem] = elem(' ')

  def whiteSpace: Parser[Unit] = rep(wsc) ^^^ ()

  def anyChar: Parser[Elem] = elem("Any Char", c => c != '\032')

  lazy val blankLine = rep1(rep(' ') ~ newline)

  lazy val EOF = not(anyChar)

  lazy val line = not(wsc) ~> rep1(not(newline) ~> anyChar) <~ (newline | EOF)

  def paragraph(indent: Int) = line ~ rep( repN(indent, ' ') ~> line) <~ opt(blankLine) ^^ {
    case line1 ~ lines => Paragraph((line1 :: lines).map{_.mkString}.mkString(" "))
  }

  lazy val bulletLead: Parser[Bullet] = rep(space) ~ bullet ~ rep1(space)^^ {
    case bulletIndent ~ bulletChar ~ bodyIndent =>
      Bullet(bulletIndent.length, bulletChar, bodyIndent.length)
  }

  def bulletItem(indent: Int, bulletIndent: Int, c: Char, bodyIndent: Int): Parser[BulletItem] = {
    val totalIndent = indent + bulletIndent + 1 + bodyIndent
    repN(bulletIndent, ' ') ~ c ~ repN(bodyIndent, ' ') ~ block(totalIndent) ~
    rep(repN(totalIndent, ' ') ~> block(totalIndent)) ^^ {
      case _ ~ block1 ~ blocks => BulletItem(block1 :: blocks)
    }
  }

  def fixedIndentBulletList(indent: Int, bulletIndent: Int, c: Char, bodyIndent: Int): Parser[BulletList] =
    bulletItem(indent, bulletIndent, c, bodyIndent) ~
    rep(bulletItem(0, indent + bulletIndent, c, bodyIndent)) ^^ {
      case item1 ~ items =>
        BulletList(indent + bulletIndent + 1 + bodyIndent, item1 :: items)
    }

  def bulletList(indent: Int): Parser[BulletList] = Parser { in =>
    bulletLead(in) match {
      case s @ Success(v, _) =>
        fixedIndentBulletList(indent, v.bulletIndent, v.c, v.bodyIndent)(in)
      case e @ Error(msg, _) => Error(msg, in)
      case f @ Failure(msg, _) => Failure(msg, in)
    }
  }

  def block(indent: Int) = bulletList(indent) | paragraph(indent)

}

abstract class reStructuredText

abstract class Block extends reStructuredText

abstract class Inline extends reStructuredText

abstract class Raw extends reStructuredText

case class Separator(c: Char, length: Int) extends Raw

case class Section(title: String, level: Int) extends Block

case class Paragraph(text: String) extends Block

case class Bullet(bulletIndent: Int, c: Char, bodyIndent: Int) extends Raw

case class BulletItem(content: List[Block]) extends Block

case class BulletList(level: Int, items: List[BulletItem]) extends Block
