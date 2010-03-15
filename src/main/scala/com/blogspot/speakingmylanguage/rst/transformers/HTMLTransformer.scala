package com.blogspot.speakingmylanguage.rst.transformers

import scala.xml._

object HTMLTransformer {
  def convert(rst: reStructuredText): NodeSeq = {
    rst match {
      case s: Section => <h1>{s.title}</h1>
    }
  }
  
  def convert(section: Section, depth: Int): NodeSeq = {
    Elem(null, section.title, Null, TopScope, Nil: _*)
  }
}
