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
      ) must beLike {
        case Success(Section("test", 1), _) => true
      }
    }

    "parse inset markup" in {
      new rstParser().
      title("""|=====
               | test
               |=====""".stripMargin
      ) must beLike {
        case Success(Section("test", 1), _) => true
      }
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
      ) must beLike {
        case Success(Section("test", 1), _) => true
      }
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
      ) must beLike {
        case Success(List(Section("test", 1),
                          Section("test2", 2),
                          Section("test3", 1),
                          Section("test4", 3)), _) => true
      }
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
      ) must beLike {
        case Success(List(Section("test", 1),
                          Paragraph("text"),
                          Paragraph("more text")), _) => true
      }
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
      ) must beLike {
        case Success(List(Section("sec1", 1),
                          Paragraph("text"),
                          Paragraph("more text"),
                          Section("sec2", 2),
                          Paragraph("subsection text"),
                          Section("sec3", 1),
                          Paragraph("another level 1 section")), _) => true
      }
    }
  }

  "paragraphs" should {
    "end in a blank line" in {
      paragraph(0)("text\n\n") must beLike {
        case Success(Paragraph("text"), _) => true
      }
    }
    "end in a line with just spaces" in {
      paragraph(0)("text\n  \n") must beLike {
        case Success(Paragraph("text"), _) => true
      }
    }
    "end at the end of the text" in {
      paragraph(0)("text") must beLike {
        case Success(Paragraph("text"), _) => true
      }
    }
    "work for expected indent for consecutive lines" in {
      paragraph(2)("""|line1
                      |  line2""".stripMargin
      ) must beLike {
        case Success(Paragraph("line1 line2"), _) => true
      }
    }
    "stop parsing for different indent for consecutive lines" in {
      paragraph(2)("""|line1
                      |   line2
                      |""".stripMargin
      ) must beLike {
        case Success(Paragraph("line1"), _) => true
      }
    }
  }

  "bullet lists" should {
    "parse items on consecutive lines" in {
      bulletList(0)("""|* item1
                       |* item2""".stripMargin
      ) must beLike {
        case Success(BulletList(2, List(BulletItem(List(Paragraph("item1"))),
                                        BulletItem(List(Paragraph("item2"))))), _) => true
      }
    }

    "parse items separated by blank lines" in {
      bulletList(0)("""|* item1
                       |
                       |* item2""".stripMargin
      ) must beLike {
        case Success(BulletList(2, List(BulletItem(List(Paragraph("item1"))),
                                        BulletItem(List(Paragraph("item2"))))), _) => true
      }
    }

    "parse items of multi-line paragraphs" in {
      bulletList(0)("""|* line1
                       |  line2""".stripMargin
      ) must beLike {
        case Success(BulletList(2, List(BulletItem(List(Paragraph("line1 line2"))))), _) => true
      }
    }

    "parse items with multiple paragraphs" in {
      bulletList(0)("""|* paragraph 1 line 1
                       |  paragraph 1 line 2
                       |
                       |  paragraph 2""".stripMargin
      ) must beLike {
        case Success(BulletList(2, List(BulletItem(List(
          Paragraph("paragraph 1 line 1 paragraph 1 line 2"),
          Paragraph("paragraph 2"))))), _) => true
      }
    }

    "parse items containing other bullet lists" in {
      bulletList(0)("""|* - item1
                       |  - item2""".stripMargin
      ) must beLike {
        case Success(BulletList(2, List(BulletItem(List(BulletList(4, List(
          BulletItem(List(Paragraph("item1"))),
          BulletItem(List(Paragraph("item2"))))))))), _) => true
      }
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
      ) must beLike {
        case Success(
          BulletList(2, List(
            BulletItem(List(
              Paragraph("list1 item1"),
              BulletList(4, List(
                BulletItem(List(
                  Paragraph("list2 item1 paragraph1 line1 list2 item1 paragraph1 line2"),
                  Paragraph("list2 item1 paragraph2")
                )),
                BulletItem(List(
                  Paragraph("list2 item2 paragraph1")
                ))
              )),
              Paragraph("list1 item1 paragraph")
            )),
            BulletItem(List(
              Paragraph("list1 item2")
            ))
          )), _) => true
      }
    }
  }
  
  "inline markup" should {
    "parse simple emphasis" in {
      emph("*some text* ") must beLike {
        case Success(Emph("some text"), _) => true
      }
    }
    
    "parse combination of emphasis, text and strong emphasis" in {
      par("*emph* then text and **strong** ") must beLike {
        case Success(List(
          Emph("emph"),
          PlainText(" then text and "),
          Strong("strong"),
          PlainText(" ")
        ), _) => true
      }
    }
    
    "parse text with a single asterisk as plain text" in {
      par("an * asterisk ") must beLike {
        case Success(List(PlainText("an * asterisk ")), _) => true
      }
    }
    
    "parse text with no asterisks as plain text" in {
      plainText("plain text ") must beLike {
        case Success(PlainText("plain text "), _) => true
      }
    }
    
    "start-strings must not be preceded by a word character" in {
      par("a*emph* ") must beLike {
        case Success(List(PlainText("a*emph* ")), _) => true
      }
    }
    
    "start-strings must not be followed by whitespace" in {
      par("a * text* ") must beLike {
        case Success(List(PlainText("a * text* ")), _) => true
      }
    }
    
    "end-strings must not be preceded by whitespace" in {
      par("*text * ") must beLike {
        case Success(List(PlainText("*text * ")), _) => true
      }
    }
    
    "there can be an asterisk inside an emphasis" in {
      par("*begin * end* ") must beLike {
        case Success(List(
          Emph("begin * end"),
          PlainText(" ")
        ), _) => true
      }
    }
    
    "end-strings must not be followed by a word character" in {
      par("*emph*x ") must beLike {
        case Success(List(PlainText("*emph*x ")), _) => true
      }
    }
    
  }
}

