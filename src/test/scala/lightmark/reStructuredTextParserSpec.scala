package lightmark

import org.specs._
import reStructuredTextParser._

class reStructuredTextParserSpec extends Specification {
  type rstParser = reStructuredTextParser

  "overline section title" should {
    "parse valid markup" in {
      new rstParser().
      title("""|====
               |test
               |====""".stripMargin
      ).get must beEqualTo(Section("test", 1))
    }

    "parse inset markup" in {
      new rstParser().
      title("""|=====
               | test
               |=====""".stripMargin
      ).get must beEqualTo(Section("test", 1))
    }

    "fail shorter separator" in {
      new rstParser().
      title("""|--
               |test
               |----""".stripMargin
      ) must beLike {
        case Failure(_, _) => true
      }
    }

    "fail unequal separators" in {
      new rstParser().
      title("""|-----
               |test
               |------""".stripMargin
      ) must beLike {
        case Failure(_, _) => true
      }
    }

    "fail diffirent characters in one separator" in {
      new rstParser().
      title("""|==-=
               |test
               |====""".stripMargin
      ) must beLike {
        case Failure(_, _) => true
      }
    }

    "fail separators with different characters in each" in {
      new rstParser().
      title("""|----
               |test
               |====""".stripMargin
      ) must beLike {
        case Failure(_, _) => true
      }
    }
  }

  "underline section title" should {
    "parse valid markup" in {
      new rstParser().
      title("""|test
               |====""".stripMargin
      ).get must beEqualTo(Section("test", 1))
    }

    "fail inset markup" in {
      new rstParser().
      title("""| test
               |=====""".stripMargin
      ) must beLike {
        case Failure(_, _) => true
      }
    }

    "fail shorter separator" in {
      new rstParser().
      title("""|test
               |--""".stripMargin
      ) must beLike {
        case Failure(_, _) => true
      }
    }

    "fail diffirent characters in one separator" in {
      new rstParser().
      title("""|test
               |=-==""".stripMargin
      ) must beLike {
        case Failure(_, _) => true
      }
    }

  }

  "nested structures" should {
    "match multiple section titles" in {
      new rstParser().
      rst("""|test
             |====
             |test2
             |-----
             |test3
             |=====
             |test4
             |+++++""".stripMargin
      ).get must beEqualTo(List(
        Section("test", 1),
        Section("test2", 2),
        Section("test3", 1),
        Section("test4", 3)))
    }
    "match a section with paragraphs" in {
      new rstParser().
      rst("""|test
             |====
             |text
             |
             |more text
             |
             |""".stripMargin
      ).get must beEqualTo(List(
        Section("test", 1),
        FormattedParagraph(List(PlainText("text"))),
        FormattedParagraph(List(PlainText("more text")))))
    }
    "match multiple sections with paragraphs" in {
      new rstParser().
      rst("""|sec1
             |====
             |text
             |
             |more text
             |
             |sec2
             |----
             |
             |subsection text
             |
             |sec3
             |====
             |another level 1 section""".stripMargin
      ).get must beEqualTo(List(
        Section("sec1", 1),
        FormattedParagraph(List(PlainText("text"))),
        FormattedParagraph(List(PlainText("more text"))),
        Section("sec2", 2),
        FormattedParagraph(List(PlainText("subsection text"))),
        Section("sec3", 1),
        FormattedParagraph(List(PlainText("another level 1 section")))))
    }
  }

  "paragraphs" should {
    "end in a blank line" in {
      paragraph(0)("text\n\n").get must beEqualTo(Paragraph("text"))
    }
    "end in a line with just spaces" in {
      paragraph(0)("text\n  \n").get must beEqualTo(Paragraph("text"))
    }
    "end at the end of the text" in {
      paragraph(0)("text").get must beEqualTo(Paragraph("text"))
    }
    "work for expected indent for consecutive lines" in {
      paragraph(2)("""|line1
                      |  line2""".stripMargin
      ).get must beEqualTo(Paragraph("line1 line2"))
    }
    "stop parsing for different indent for consecutive lines" in {
      paragraph(2)("""|line1
                      |   line2
                      |""".stripMargin
      ).get must beEqualTo(Paragraph("line1"))
    }
  }

  "bullet lists" should {
    "parse items on consecutive lines" in {
      bulletList(0)("""|* item1
                       |* item2""".stripMargin
      ).get must beEqualTo(BulletList(2, List(
        BulletItem(List(FormattedParagraph(List(PlainText("item1"))))),
        BulletItem(List(FormattedParagraph(List(PlainText("item2"))))))))
    }

    "parse items separated by blank lines" in {
      bulletList(0)("""|* item1
                       |
                       |* item2""".stripMargin
      ).get must beEqualTo(BulletList(2, List(
        BulletItem(List(FormattedParagraph(List(PlainText("item1"))))),
        BulletItem(List(FormattedParagraph(List(PlainText("item2"))))))))
    }

    "parse items of multi-line paragraphs" in {
      bulletList(0)("""|* line1
                       |  line2""".stripMargin
      ).get must beEqualTo(BulletList(2, List(
        BulletItem(List(FormattedParagraph(List(PlainText("line1 line2"))))))))
    }

    "parse items with multiple paragraphs" in {
      bulletList(0)("""|* paragraph 1 line 1
                       |  paragraph 1 line 2
                       |
                       |  paragraph 2""".stripMargin
      ).get must beEqualTo(BulletList(2, List(BulletItem(List(
        FormattedParagraph(List(PlainText("paragraph 1 line 1 paragraph 1 line 2"))),
        FormattedParagraph(List(PlainText("paragraph 2"))))))))
    }

    "parse items containing other bullet lists" in {
      bulletList(0)("""|* - item1
                       |  - item2""".stripMargin
      ).get must beEqualTo(BulletList(2, List(BulletItem(List(BulletList(4, List(
        BulletItem(List(FormattedParagraph(List(PlainText("item1"))))),
        BulletItem(List(FormattedParagraph(List(PlainText("item2"))))))))))))
    }

    "parse items of lists of multiple multi-line paragraphs" in {
      bulletList(0)("""|* list1 item1
                       |
                       |  - list2 item1 paragraph1 line1
                       |    list2 item1 paragraph1 line2
                       |
                       |    list2 item1 paragraph2
                       |  - list2 item2 paragraph1
                       |
                       |  list1 item1 paragraph
                       |
                       |* list1 item2""".stripMargin
      ).get must beEqualTo(
        BulletList(2, List(
          BulletItem(List(
            FormattedParagraph(List(PlainText("list1 item1"))),
            BulletList(4, List(
              BulletItem(List(
                FormattedParagraph(List(PlainText("list2 item1 paragraph1 line1 list2 item1 paragraph1 line2"))),
                FormattedParagraph(List(PlainText("list2 item1 paragraph2")))
              )),
              BulletItem(List(
                FormattedParagraph(List(PlainText("list2 item2 paragraph1")))
              ))
            )),
            FormattedParagraph(List(PlainText("list1 item1 paragraph")))
          )),
          BulletItem(List(
            FormattedParagraph(List(PlainText("list1 item2")))
          ))
        )))
    }
  }
  
  "inline markup" should {
    "parse simple emphasis" in {
      emph("*some text* ").get must beEqualTo(Emph("some text"))
    }
    
    "parse combination of emphasis, text and strong emphasis" in {
      par("*emph* then text and **strong** ").get must beEqualTo(List(
        Emph("emph"),
        PlainText(" then text and "),
        Strong("strong"),
        PlainText(" ")
      ))
    }
    
    "parse text with a single asterisk as plain text" in {
      par("an * asterisk ").get must beEqualTo(List(PlainText("an * asterisk ")))
    }
    
    "parse text with no asterisks as plain text" in {
      plainText("plain text ").get must beEqualTo(PlainText("plain text "))
    }
    
    "start-strings must not be preceded by a word character" in {
      par("a*emph* ").get must beEqualTo(List(PlainText("a*emph* ")))
    }
    
    "start-strings must not be followed by whitespace" in {
      par("a * text* ").get must beEqualTo(List(PlainText("a * text* ")))
    }
    
    "end-strings must not be preceded by whitespace" in {
      par("*text * ").get must beEqualTo(List(PlainText("*text * ")))
    }
    
    "there can be an asterisk inside an emphasis" in {
      par("*begin * end* ").get must beEqualTo(List(
        Emph("begin * end"),
        PlainText(" ")
      ))
    }
    
    "end-strings must not be followed by a word character" in {
      par("*emph*x ").get must beEqualTo(List(PlainText("*emph*x ")))
    }
  }

  "escapes" should {
    val format = formattedParagraph(0)
    "escaped start-string shouldn't start inline" in {
      format("""\*emph*""").get must beEqualTo(FormattedParagraph(List(
        PlainText("*emph*"))))
    }

    "escaped end-string shouldn't end inline" in {
      format("""*emph\* end*""").get must beEqualTo(FormattedParagraph(List(
        Emph("emph* end"))))
    }

    "escape after end-string shouldn't end inline" in {
      format("""*emph*\s""").get must beEqualTo(FormattedParagraph(List(
        Emph("emph"),
        PlainText("s"))))
    }

    "escaped backslash shouldn't escape start-string" in {
      format("""\\*emph*""").get must beEqualTo(FormattedParagraph(List(
        PlainText("""\*emph*"""))))
    }

    "escaped backslash shouldn't escape end-string" in {
      format("""*emph\\*""").get must beEqualTo(FormattedParagraph(List(
        Emph("""emph\"""))))
    }
  }

  "block quotes" should {
    "parse simple indented block" in {
      (block(0)*)("""|text
                     |
                     |  quoted block""".stripMargin
      ).get must beEqualTo(List(
        FormattedParagraph(List(PlainText("text"))),
        Quote(List(FormattedParagraph(List(PlainText("quoted block")))))))
    }

    "parse indented block with inline formatting" in {
      (block(0)*)("""|text
                     |
                     |  block *with inline* formatting""".stripMargin
      ).get must beEqualTo(List(
        FormattedParagraph(List(PlainText("text"))),
        Quote(List(FormattedParagraph(List(PlainText("block "), Emph("with inline"), PlainText(" formatting")))))))
    }

    "parse indented bullet list" in {
      (block(0)*)("""|text
                     |
                     |  * item1
                     |  * item2""".stripMargin
      ).get must beEqualTo(List(
        FormattedParagraph(List(PlainText("text"))),
        Quote(List(BulletList(4, List(
          BulletItem(List(FormattedParagraph(List(PlainText("item1"))))),
          BulletItem(List(FormattedParagraph(List(PlainText("item2")))))))))))
    }
  }

  "literal blocks" should {
    "parse literal block after text with colons" in {
      formattedParagraph(0)(
        """|text::
           |
           |  literal""".stripMargin
      ).get must beEqualTo(FormattedParagraph(List(PlainText("text:")), Some("literal")))
    }

    "parse literal block after text with space before colons" in {
      formattedParagraph(0)(
        """|text ::
           |
           |  literal""".stripMargin
      ).get must beEqualTo(FormattedParagraph(List(PlainText("text")), Some("literal")))
    }

    "drop empty block with colons" in {
      formattedParagraph(0)(
        """|::
           |
           |  literal""".stripMargin
      ).get must beEqualTo(FormattedParagraph(Nil, Some("literal")))
    }

    "parse literal block with blank lines" in {
      formattedParagraph(0)(
        """|::
           |
           |  literal1
           |
           |  literal2""".stripMargin
      ).get must beEqualTo(FormattedParagraph(Nil, Some("literal1\n\nliteral2")))
    }

    "find minimum indent" in {
      formattedParagraph(0)(
        """|::
           |
           |    literal1
           |  literal2""".stripMargin
      ).get must beEqualTo(FormattedParagraph(Nil, Some("  literal1\nliteral2")))
    }

    "ignore blank lines before next paragraph" in {
      formattedParagraph(0)(
        """|::
           |
           |  literal1
           |
           |
           |next""".stripMargin
      ).get must beEqualTo(FormattedParagraph(Nil, Some("literal1")))
    }
  }

  "definition lists" should {
    "parse simple term and definition" in {
      block(0)("""|term
                  |  definition""".stripMargin
      ).get must beEqualTo(DefinitionList(List(DefinitionItem(
        List(PlainText("term")),
        List(FormattedParagraph(List(PlainText("definition"))))
      ))))
    }

    "parse term and definition with multiple paragraphs" in {
      block(0)("""|term
                  |  paragraph1
                  |
                  |  paragraph2""".stripMargin
      ).get must beEqualTo(DefinitionList(List(DefinitionItem(
        List(PlainText("term")),
        List(FormattedParagraph(List(PlainText("paragraph1"))),
             FormattedParagraph(List(PlainText("paragraph2"))))
      ))))
    }

    "parse term and definition with bullet list" in {
      block(0)("""|term
                  |  * item1
                  |  * item2""".stripMargin
      ).get must beEqualTo(DefinitionList(List(DefinitionItem(
        List(PlainText("term")),
        List(BulletList(4, List(
          BulletItem(List(FormattedParagraph(List(PlainText("item1"))))),
          BulletItem(List(FormattedParagraph(List(PlainText("item2")))))
        )))
      ))))
    }
  }
}

