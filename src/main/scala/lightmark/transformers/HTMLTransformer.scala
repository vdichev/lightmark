package lightmark.transformers

import scala.xml._
import lightmark._

object HTMLTransformer {
  def convert(rst: reStructuredText): NodeSeq = {
    rst match {
      case Section(title, level) => Elem(null, "h" + level, Null, TopScope, Text(title))
      case Paragraph(text) => <p>{text}</p>
      case FormattedParagraph(inlines) => <p>{inlines map convert}</p>
      case PlainText(content) => Text(content)
      case BulletList(_, items) => <ul>{items map convert}</ul>
      case BulletItem(content) => <li>{content map convert}</li>
      case Emph(content) => <em>{content}</em>
      case Strong(content) => <strong>{content}</strong>
      case Literal(content) => <tt>{content}</tt>
      case Quote(content) => <blockquote>{content map convert}</blockquote>
    }
  }
  
  def convert(l: List[reStructuredText]): NodeSeq = {
    l flatMap { HTMLTransformer convert _}
  }
  
  def convert(section: Section, depth: Int): NodeSeq = {
    Elem(null, section.title, Null, TopScope, Nil: _*)
  }
}
