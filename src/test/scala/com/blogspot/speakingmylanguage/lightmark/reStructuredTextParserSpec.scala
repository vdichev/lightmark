package com.blogspot.speakingmylanguage.lightmark

import org.specs._
import reStructuredTextParser._

class reStructuredTextParserSpec extends Specification {
  "overline section title" should {
    "parse valid markup" in {
      title("""|====
               |test
               |====""".stripMargin
      ) must beLike {
        case Success(Section("test", 1), _) => true
      }
    }
    
    "parse inset markup" in {
      title("""|=====
               | test
               |=====""".stripMargin
      ) must beLike {
        case Success(Section("test", 1), _) => true
      }
    }
    
    "fail shorter separator" in {
      title("""|--
               |test
               |----""".stripMargin
      ) must beLike {
        case Failure(_, _) => true
      }
    }
    
    "fail unequal separators" in {
      title("""|-----
               |test
               |------""".stripMargin
      ) must beLike {
        case Failure(_, _) => true
      }
    }
    
    "fail diffirent characters in one separator" in {
      title("""|==-=
               |test
               |====""".stripMargin
      ) must beLike {
        case Failure(_, _) => true
      }
    }
    
    "fail separators with different characters in each" in {
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
      title("""|test
               |====""".stripMargin
      ) must beLike {
        case Success(Section("test", 1), _) => true
      }
    }

    "fail inset markup" in {
      title("""| test
               |=====""".stripMargin
      ) must beLike {
        case Failure(_, _) => true
      }
    }

    "fail shorter separator" in {
      title("""|test
               |--""".stripMargin
      ) must beLike {
        case Failure(_, _) => true
      }
    }
    
    "fail diffirent characters in one separator" in {
      title("""|test
               |=-==""".stripMargin
      ) must beLike {
        case Failure(_, _) => true
      }
    }
    
  }
  
  "nested structures" should {
    "match multiple section titles" in {
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
      paragraph("text\n\n") must beLike {
        case Success(Paragraph("text"), _) => true
      }
    }
    "end in a line with just spaces" in {
      paragraph("text\n  \n") must beLike {
        case Success(Paragraph("text"), _) => true
      }
    }
    "end at the end of the text" in {
      paragraph("text") must beLike {
        case Success(Paragraph("text"), _) => true
      }
    }
  }
}
