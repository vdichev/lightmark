package com.blogspot.speakingmylanguage.rst

import org.specs._
import reStructuredTextParser._

class reStructuredTextParserSpec extends Specification {
  "overline section title" should {
    "parse valid markup" in {
      title("====\ntest\n====") must beLike { case Success(Section("test"), _) => true }
    }
    
    "parse inset markup" in {
      title("=====\n test\n=====") must beLike { case Success(Section("test"), _) => true }
    }
    
    "fail shorter separator" in {
      title("--\ntest\n----") must beLike { case Failure(_, _) => true }
    }
    
    "fail unequal separators" in {
      title("-----\ntest\n------") must beLike { case Failure(_, _) => true }
    }
    
    "fail diffirent characters in one separator" in {
      title("==-=\ntest\n====") must beLike { case Failure(_, _) => true }
    }
    
    "fail separators with different characters in each" in {
      title("----\ntest\n====") must beLike { case Failure(_, _) => true }
    }
  }
  
  "undeline section title" should {
    "parse valid markup" in {
      title("test\n====") must beLike { case Success(Section("test"), _) => true }
    }

    "fail inset markup" in {
      title(" test\n=====") must beLike { case Failure(_, _) => true }
    }

    "fail shorter separator" in {
      title("test\n--") must beLike { case Failure(_, _) => true }
    }
    
    "fail diffirent characters in one separator" in {
      title("test\n=-==") must beLike { case Failure(_, _) => true }
    }
    
  }
}
