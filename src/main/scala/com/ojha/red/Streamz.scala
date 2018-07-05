package com.ojha.red

import com.ojha.red.Stream._

import scala.language.postfixOps

sealed trait Stream[+A] {

  def append[B >: A](s: => Stream[B]): Stream[B] =
    this.foldRight(s)((h, cl) => cons(h, cl))

  def filter(f: A => Boolean): Stream[A] =
    this.foldRight(empty[A])((h, cl) => if (f(h)) cons(h, cl.filter(f)) else cl.filter(f))

  //  def map[B](f: A => B): Stream[B] =
  //    this.foldRight(empty[B])((a, cl) => cons(f(a), cl))

  def map[B](f: A => B): Stream[B] = unfold[B, Stream[A]](this) {
    case Empty => None
    case Cons(h, t) => Some((f(h()), t()))
  }

  def forAll(f: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => f(h()) && t().forAll(f)
  }

  //  def takeWhile(p: A => Boolean): Stream[A] =
  //    this.foldRight[Stream[A]](Empty)((h, cl) => if (p(h)) cons(h, cl) else cl)

  def takeWhile(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  // cant handle invalid case with non zero n on empty
  // cant handle large inputs
  def drop(n: Int): Stream[A] = this match {
    case x if n == 0 => x
    case Cons(_, t) => t().drop(n - 1)
  }

  // doesnt handle cal/ing non zero take on empty stream
  // cant handle large inputs
  //  def take(n: Int): Stream[A] = this match {
  //    case _ if n == 0 => Empty
  //    case Cons(h, tail) => cons(h(), tail().take(n - 1))
  //  }

  def take(n: Int): Stream[A] = unfold((this, 0)) {
    case (Cons(h, t), i) if i < n => Some((h(), (t(), i + 1)))
    case _ => None
  }

  def headOption: Option[A] =
    foldRight[Option[A]](None)((h, _) => Some(h))

  def headOption2: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Option(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() +: t().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = this.foldRight(false)((a, b) => p(a) || b)

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def zipWith[B](bs: Stream[B]): Stream[(A, B)] = unfold((this, bs)) {
    case (Cons(ha, ta), Cons(hb, tb)) => Some((ha(), hb()), (ta(), tb()))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(ha, ta), Cons(hb, tb)) => {
        val state = (ta(), tb())
        val currentValue = (Some(ha()), Some(hb()))
        Some(currentValue, state)
      }
      case (Empty, Cons(hb, tb)) => {
        val state = (Empty, tb())
        val currentValue = (None, Some(hb()))
        Some(currentValue, state)
      }
      case (Cons(ha, ta), Empty) => {
        val state = (ta(), Empty)
        val currentValue = (Some(ha()), None)
        Some(currentValue, state)
      }
      case _ => None
    }

  def startsWith[A](s: Stream[A]): Boolean = (this, s) match {
    case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => t1().startsWith(t2())
    case (_, Empty) => true
    case _ => false
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case c: Cons[A] => Some((c, c.tail()))
    case _ => None
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = as length match {
    case 0 => empty
    case _ => cons(as.head, apply(as.tail: _*))
  }

  //  def constant[A](a: A): Stream[A] = {
  //    lazy val s: Stream[A] = Stream.cons(a, s)
  //    s
  //  }

  def constant[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def from(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def fibs: Stream[Int] = unfold((0, 1))(c => Some((c._1, (c._2, c._1 + c._2))))

  private def fibs(n: Int, m: Int): Stream[Int] = cons(m, fibs(m, m + n))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

}


