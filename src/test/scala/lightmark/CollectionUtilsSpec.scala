package lightmark

import org.specs._

object CollectionUtilsSpec extends Specification {
  import CollectionUtils.slide2padded
  val segment = CollectionUtils.segment[Int](_ != _) _
  "segment" should {
    "produce single list out of non-repeating elements" in {
      segment(slide2padded(List(1, 2, 3, 4))) must beEqualTo(List(List(1, 2, 3, 4)))
    }

    "stop on leading repeated elements" in {
      segment(slide2padded(List(1, 1, 2))) must beEqualTo(List(List(1), List(1, 2)))
    }

    "stop on trailing repeated elements" in {
      segment(slide2padded(List(1, 2, 2))) must beEqualTo(List(List(1, 2), List(2)))
    }

    "work with multiple repeated elements" in {
      segment(slide2padded(List(1, 2, 2, 3, 3, 3))) must beEqualTo(List(List(1, 2), List(2, 3), List(3), List(3)))
    }

    "work with single-element lists" in {
      segment(slide2padded(List(1))) must beEqualTo(List(List(1)))
    }

    "work with empty lists" in {
      segment(slide2padded(Nil)) must beEqualTo(Nil)
    }
  }
}
