package lightmark

import _root_.scala.util.parsing.combinator.{Parsers, ImplicitConversions}
import _root_.scala.util.parsing.input._
import _root_.scala.xml.{Elem => XmlElem, MetaData, NodeSeq, Null, Text, TopScope, Unparsed, UnprefixedAttribute, Group, Node}
import _root_.scala.collection.mutable.HashMap

import java.util.regex.Pattern

class reStructuredTextParser {
  import reStructuredTextParser._

  // A structure indicating which character underlines each section level
  var headingLevels = List.empty[Char]

  def getLevel(c: Char): Int = {
    if (!(headingLevels contains c))
      headingLevels :+= c
    (headingLevels findIndexOf { c == _ }) + 1
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

  def parse(string: String) = rst(string)
}

object reStructuredTextParser extends Parsers with ImplicitConversions {

  implicit def strToInput(in: String): Input = new CharArrayReader(in.toCharArray)
  implicit def str2chars(s: String): List[Char] = augmentString(s).toList

  type Elem = Char

  val punctRegex = Pattern.compile("""\p{Punct}""")

  def isPunct(c: Char) = punctRegex.matcher(c.toString).matches

  lazy val punct: Parser[Elem] = elem("separator", isPunct)

  lazy val separator = repFirst(punct) ^^ {
    case s => Separator(s.head, s.length)
  }

  // A parser which captures the first element and matches repetitions
  // of the same character
  def repFirst(p: Parser[Elem]): Parser[List[Elem]] = guard(p) >> (v => rep(v))

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

  lazy val spaceEOF = rep(wsc) ~ EOF

  lazy val line = not(wsc) ~> rep1(not(newline) ~> anyChar) <~ newlineOrEOF

  def paragraph(indent: Int) = line ~ rep( repN(indent, ' ') ~> line) ^^ {
    case line1 ~ lines =>
      val text = (line1 :: lines).map(_.mkString).mkString(" ")
      Paragraph(text)
  }

  def resetInputForResult[T](result: ParseResult[T]) = Parser { in =>
    result match {
      case Success(res, _) => Success(res, in)
      case noSuccess => noSuccess
    }
  }

  def formattedParagraph(indent: Int): Parser[FormattedParagraph] =
    for (p <- paragraph(indent);
         inline <- resetInputForResult(par(p.text + "\n"))
    ) yield FormattedParagraph(inline)

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

  def bulletList(indent: Int): Parser[BulletList] = guard(bulletLead) >> { v =>
    fixedIndentBulletList(indent, v.c, v.bodyIndent)
  }

  def block(indent: Int) = bulletList(indent) | formattedParagraph(indent) <~ opt(blankLines)

  /**
    The inline markup start-string and end-string recognition rules are as follows. If any of the conditions are not met, the start-string or end-string will not be recognized or processed.
    
    http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#inline-markup
   */
  def inline(s: String)(constructor: String => Inline) =
    accept(s) ~>
    // Inline markup start-strings must be immediately followed by non-whitespace.
    (not(wsc) ~>
      // Inline markup end-strings must be immediately preceded by non-whitespace.
      rep1(wsc ~ accept(s) |
        // Inline markup end-strings must end a text block or be immediately followed by whitespace or one of the following ASCII characters:
        // ' " ) ] } > - / : . , ; ! ? \
        not(accept(s) ~ postInline) ~> anyChar) ) <~
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

  lazy val literal = inline("``") { s => Literal(s) }

  lazy val inlineElems = strong | emph | literal

  lazy val plainText = rep(not(preInline ~ inlineElems) ~ not(spaceEOF) ~> anyChar) ~ (preInline | failure("preInline expected")) ^^ {
    case text ~ lastChar if lastChar.toString != "\n" =>
      PlainText(text.mkString + lastChar)
    case text ~ _ => PlainText(text.mkString)
  }
  
  lazy val par = rep1(inlineElems | plainText)

  def main(args: Array[String]) {
    import io.Source
    val contents = Source.fromFile(args.head).getLines.mkString("\n")
    val rst = new reStructuredTextParser().parse(contents)
    rst match {
      case Success(result, _) =>
        val html = transformers.HTMLTransformer.convert(result)

        print(html)
    }
  }
}

abstract class reStructuredText

abstract class Block extends reStructuredText

abstract class Inline(val content: String) extends reStructuredText

abstract class Raw extends reStructuredText

case class Separator(c: Char, length: Int) extends Raw

case class Section(title: String, level: Int) extends Block

case class Paragraph(text: String) extends Block

case class FormattedParagraph(inline: List[Inline]) extends Block

case class Bullet(c: Char, bodyIndent: Int) extends Raw

case class BulletItem(content: List[Block]) extends Block

case class BulletList(level: Int, items: List[BulletItem]) extends Block

case class PlainText(override val content: String) extends Inline(content)

case class Emph(override val content: String) extends Inline(content)

case class Strong(override val content: String) extends Inline(content)

case class Literal(override val content: String) extends Inline(content)
