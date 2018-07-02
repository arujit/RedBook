package Redbook.Errors_Exception


import scala.{Option=>_}

sealed  trait Option[+A]
{
  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def map[B](f: A => B): Option[B] = this match
    {
      case None => None
      case Some(a) => Some(f(a))
    }

  //def flatmap[B](f: A => Option[B] ): Option[B] = ???

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob


  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
  def filter_1(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)


}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("function is not activated!")): Int)
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    {
      a flatMap (aa => b map (bb => f(aa, bb)))
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }


  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }


  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    {
      traverse(a)(x => x)
    }
}
