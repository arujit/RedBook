package MyList

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }


  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](first_list: List[A], second_list: List[A]): List[A] = {
    first_list match {
      case Nil => second_list
      case Cons(head, tail) => Cons(head, append(tail, second_list))
    }
  }

  def tail[A](entire_list: List[A]): List[A] = {
    entire_list match {
      case Nil => sys.error("Not valid")
      case Cons(_, end) => end

    }
  }

  def setHead[A](entire_list: List[A], element_to_be_sub: A):List[A] = {
    entire_list match {
      case Nil => sys.error("Not Valid")
      case Cons(_, tail)  => Cons(element_to_be_sub, tail)
    }
  }

  def drop[A](lis: List[A], n: Int):List[A] = {
    if (n <= 0) lis
    else lis match{
      case Nil => Nil
      case Cons(_, tail) => drop(tail, n-1)
    }
  }
  def init[A](lis: List[A]) :List[A] = {
    lis match {
      case Nil => sys.error("Not Valid")
      case Cons(_,Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }
  }
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)



}
