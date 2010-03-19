package com.blogspot.speakingmylanguage.rst.transformers

import scala.xml._
import org.specs._
import HTMLTransformer._

class HTMLTransformerSpec extends Specification {
  "HTML Transformer" should {
    "convert section level 1 into h1 tag" in {
      convert(Section("mytitle", 1)) must beEqualTo(<h1>mytitle</h1>)
    }
    "convert section level 2 into h2 tag" in {
      convert(Section("mytitle", 2)) must beEqualTo(<h2>mytitle</h2>)
    }
    "convert sections 1 & 2 into h1 tag followed by h2 tag" in {
      convert(List(Section("sec1", 1), Section("sec2", 2))) must beEqualTo(<h1>sec1</h1><h2>sec2</h2>: NodeSeq)
    }
    "convert a paragraph in p tag" in {
      convert(Paragraph("para text")) must beEqualTo(<p>para text</p>)
    }
  }
}

