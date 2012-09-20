package lightmark

object CollectionUtils {
  def segment[A](accept: (A, A) => Boolean)(lol: Seq[Seq[A]]): Seq[Seq[A]] =
    if (lol.isEmpty) Nil
    else {
      val (prefix, suffix) = lol.span(xs => accept(xs.head, xs.last))
      Seq((prefix :+ suffix.head) map (_.head)) ++ segment(accept)(suffix.tail)
    }

  // append the duplicated last element of the last seq in a seq
  // (such as the seq of seqs produced from sliding(2))
  // to avoid checking for corner cases in segment
  def slide2padded[A](seq: Seq[A]): Seq[Seq[A]] = seq match {
    case Nil => Nil
    case Seq(x) => Seq(Seq.fill(2)(x))
    case list =>
      val slideList = list.sliding(2).toList
      slideList :+ Seq.fill(2)(slideList.last.last)
  }

}
