package lightmark.transformers

import lightmark._

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
    "convert Emph to em tag" in {
      convert(Emph("emphasized text")) must beEqualTo(<em>emphasized text</em>)
    }
    "convert Strong to strong tag" in {
      convert(Strong("strong text")) must beEqualTo(<strong>strong text</strong>)
    }
    "convert Literal to tt tag" in {
      convert(Literal("literal text")) must beEqualTo(<tt>literal text</tt>)
    }
    "convert bullet list to ul and li tags" in {
      val bulletList = BulletList(2, List(
        BulletItem(List(FormattedParagraph(List(PlainText("item1"))))),
        BulletItem(List(FormattedParagraph(List(PlainText("item2")))))))
      convert(bulletList) must ==/(<ul><li><p>item1</p></li><li><p>item2</p></li></ul>)
    }
    "convert block quotes to blockquote tag" in {
      convert(Quote(List(FormattedParagraph(List(PlainText("hey")))))) must ==/(
        <blockquote><p>hey</p></blockquote>)
    }
  }
}

