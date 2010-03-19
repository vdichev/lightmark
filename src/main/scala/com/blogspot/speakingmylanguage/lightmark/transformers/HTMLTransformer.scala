package com.blogspot.speakingmylanguage.lightmark.transformers

import scala.xml._

object HTMLTransformer {
  def convert(rst: reStructuredText): NodeSeq = {
    rst match {
      case s: Section => Elem(null, "h" + s.level, Null, TopScope, Text(s.title))
      case p: Paragraph => <p>{p.text}</p>
    }
  }
  
  def convert(l: List[reStructuredText]): NodeSeq = {
    l flatMap { HTMLTransformer convert _}
  }
  
  def convert(section: Section, depth: Int): NodeSeq = {
    Elem(null, section.title, Null, TopScope, Nil: _*)
  }
}
