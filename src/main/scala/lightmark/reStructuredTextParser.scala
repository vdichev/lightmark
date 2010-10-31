package lightmark

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
  implicit def str2chars(s: String): List[Char] = stringWrapper(s).toList

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

  lazy val punctuation = elem('!') | '"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@' | '[' | '\\' | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~'

  // dashes and hyphens
  lazy val delim = elem('\u2010') | '\u2011' | '\u2012' | '\u2013' | '\u2014' | '\u00A0'
  
  lazy val preInline = elem('\'') | '"' | '(' | '[' | '{' | '<' | '-' | '/' | ':' |
    '‘' | '“' | '’' | '«' | '¡' | '¿' | wsc | delim
    
  lazy val postInline = elem('\'') | '"' | ')' | ']' | '}' | '>' | '-' | '/' | ':' |
    '.' | ',' | ';' | '!' | '?' | '\\' | '’' | '”' | '»' | wsc | delim

  lazy val bullet: Parser[Elem] = elem('*') | '+' | '-' | '•' | '‣' | '⁃'

  lazy val letter: Parser[Elem] = elem("Letter", c => Character.isLetter(c))

  lazy val newline = accept("\r\n") | '\n' | '\r'

  lazy val newlineOrEOF = accept("\r\n") | '\n' | '\r' | EOF

  def wsc(c: Char): Boolean = Character.isWhitespace(c) // ' ' || c == '\n' || c == '\r' || c == '\t'
  def wsc: Parser[Elem] = elem("wsc", wsc)

  lazy val space: Parser[Elem] = elem(' ')

  def whiteSpace: Parser[Unit] = rep(wsc) ^^^ ()

  def anyChar: Parser[Elem] = elem("Any Char", c => c != '\032')

  lazy val blankLines = rep1(rep(' ') ~ newline) | rep(' ') ~ EOF

  lazy val EOF = accept('\032')

  lazy val line = not(wsc) ~> rep1(not(newline) ~> anyChar) <~ newlineOrEOF

  def paragraph(indent: Int) = line ~ rep( repN(indent, ' ') ~> line) ^^ {
    case line1 ~ lines => Paragraph((line1 :: lines).map{_.mkString}.mkString(" "))
  }

  lazy val bulletLead: Parser[Bullet] = bullet ~ rep1(space)^^ {
    case bulletChar ~ bodyIndent =>
      Bullet(bulletChar, bodyIndent.length)
  }

  def bulletItem(indent: Int, c: Char, bodyIndent: Int): Parser[BulletItem] = {
    val totalIndent = indent + 1 + bodyIndent
    c ~ repN(bodyIndent, ' ') ~ block(totalIndent) ~
    rep(repN(totalIndent, ' ') ~> block(totalIndent)) ^^ {
      case _ ~ block1 ~ blocks => BulletItem(block1 :: blocks)
    }
  }

  def fixedIndentBulletList(indent: Int, c: Char, bodyIndent: Int): Parser[BulletList] =
    bulletItem(indent, c, bodyIndent) ~
    rep(repN(indent, ' ') ~> bulletItem(indent, c, bodyIndent)) ^^ {
      case item1 ~ items =>
        BulletList(indent + 1 + bodyIndent, item1 :: items)
    }

  def bulletList(indent: Int): Parser[BulletList] = Parser { in =>
    bulletLead(in) match {
      case s @ Success(v, _) =>
        fixedIndentBulletList(indent, v.c, v.bodyIndent)(in)
      case e @ Error(msg, _) => Error(msg, in)
      case f @ Failure(msg, _) => Failure(msg, in)
    }
  }

  def block(indent: Int) = bulletList(indent) | paragraph(indent) <~ opt(blankLines)

  // TODO: starting a block
  def inline(s: String)(constructor: String => Inline) =
    accept(s) ~>
    (not(wsc) ~>
      rep1(wsc ~ accept(s) |
        not(accept(s) ~ postInline) ~
        not(blankLines) ~> anyChar) ) <~
    accept(s) ^^ {
    case text =>
      val textString = text.foldLeft ("") { case (s, p) => p match {
          case space ~ (endString: List[Char]) => s + space + endString.mkString
          case c => s + c
        }
      }
      constructor(textString)
  }

  lazy val emph = inline("*") { s => Emph(s) }
  
  lazy val strong = inline("**") { s => Strong(s) }
  
  lazy val inlineElems = strong | emph
  
  lazy val plainText = rep(not(preInline ~ inlineElems) ~ not(blankLines) ~> anyChar) ~ (preInline | failure("preInline expected")) ^^ {
    case text ~ lastChar => PlainText(text.mkString + lastChar)
  }
  
  lazy val par = rep1(inlineElems | plainText)

}

abstract class reStructuredText

abstract class Block extends reStructuredText

abstract class Inline(val content: String) extends reStructuredText

abstract class Raw extends reStructuredText

case class Separator(c: Char, length: Int) extends Raw

case class Section(title: String, level: Int) extends Block

case class Paragraph(text: String) extends Block

case class Bullet(c: Char, bodyIndent: Int) extends Raw

case class BulletItem(content: List[Block]) extends Block

case class BulletList(level: Int, items: List[BulletItem]) extends Block

case class PlainText(override val content: String) extends Inline(content)

case class Emph(override val content: String) extends Inline(content)

case class Strong(override val content: String) extends Inline(content)
