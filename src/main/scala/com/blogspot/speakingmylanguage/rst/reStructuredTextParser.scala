package com.blogspot.speakingmylanguage.rst

import _root_.scala.util.parsing.combinator.{Parsers, ImplicitConversions}
import _root_.scala.util.parsing.input._
import _root_.scala.xml.{Elem => XmlElem, MetaData, NodeSeq, Null, Text, TopScope, Unparsed, UnprefixedAttribute, Group, Node}
import _root_.scala.collection.mutable.HashMap

import java.util.regex.Pattern

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
  
  def repFirst(p: Parser[Elem]): Parser[List[Elem]] = Parser { in =>
    p(in) match {
      case s @ Success(v, _) => rep(v)(in)
      case e @ Error(msg, _) => Error(msg, in)
      case f @ Failure(msg, _) => Failure(msg, in)
    }
  }
  
  // elem('!') | '"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@' | '[' | '\\' | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~'
  
  lazy val letter: Parser[Elem] = elem("Letter", c => Character.isLetter(c))
  
  lazy val newline = accept("\r\n".toList) | '\n' | '\r'
  
  def wsc(c: Char): Boolean = Character.isWhitespace(c) // ' ' || c == '\n' || c == '\r' || c == '\t'
  def wsc: Parser[Elem] = elem("wsc", wsc)
  
  def whiteSpace: Parser[Unit] = rep(wsc) ^^^ ()

  def anyChar: Parser[Elem] = elem("Any Char", c => c != '\032')
  
  lazy val title: Parser[reStructuredText] = overlineTitle | underlineTitle
  
  lazy val overlineTitle: Parser[reStructuredText] = separator ~ newline ~ rep1(not(newline) ~ whiteSpace ~> anyChar) ~ newline ~ separator <~ whiteSpace ^? {
    case overline ~ _ ~ text ~ _ ~ underline
      if underline.length >= text.length &&
         underline.length == overline.length &&
         overline.c == underline.c =>
           Section(text.mkString)
  }

  lazy val underlineTitle: Parser[reStructuredText] = not(wsc) ~> rep1(not(newline) ~> anyChar) ~ newline ~ separator <~ whiteSpace ^? {
    case text ~ _ ~ underline
      if underline.length >= text.length =>
        Section(text.mkString)
  }

  

  def main(args: Array[String]) {
    import transformers._
    println(HTMLTransformer.convert(title("test\n====").get))
  }
}

abstract class reStructuredText

case class Separator(c: Char, length: Int) extends reStructuredText

case class Section(title: String) extends reStructuredText
