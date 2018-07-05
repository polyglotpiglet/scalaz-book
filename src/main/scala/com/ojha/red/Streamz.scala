package com.ojha.red

import Stream._
import scala.language.postfixOps

sealed trait Stream[+A] {

  def append[B >: A](s: => Stream[B]): Stream[B] =
    this.foldRight(s)((h,cl) => cons(h, cl))

  def filter(f: A => Boolean): Stream[A] =
    this.foldRight(empty[A])((h, cl) => if (f(h)) cons(h, cl.filter(f)) else cl.filter(f))

  def map[B](f: A => B): Stream[B] =
    this.foldRight(empty[B])((a, cl) => cons(f(a), cl))

  def forAll(f: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => f(h()) && t().forAll(f)

  }

  def takeWhile(p: A => Boolean): Stream[A] =
    this.foldRight[Stream[A]](Empty)((h, cl) => if (p(h)) cons(h, cl) else cl)


  // cant handle invalid case with non zero n on empty
  // cant handle large inputs
  def drop(n: Int): Stream[A] = this match {
    case x if n == 0 => x
    case Cons(_, t) => t().drop(n-1)
  }

  // doesnt handle cal/ing non zero take on empty stream
  // cant handle large inputs
  def take(n: Int): Stream[A] = this match {
    case _ if n == 0 => Empty
    case Cons(h, tail) => cons(h(), tail().take(n -1))
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

  def exists(p: A => Boolean): Boolean = this.foldRight(false)((a,b) => p(a) || b)
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
}


