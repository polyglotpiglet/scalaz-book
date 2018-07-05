package com.ojha.red

trait CatList[+A]

object CatNil extends CatList[Nothing]

final class CatCons[A](val a: A, tail: => CatList[A]) extends CatList[A] {
  def getTail():CatList[A] = tail
}


object CatCons {
  def apply[A](a: A, tail: => CatList[A]) = new CatCons[A](a, tail)
  def unapply[A](cl: CatCons[A]): Option[(A, CatList[A])] = Option((cl.a, cl.getTail()))
}

object CatList {
  def tail[A](cl: CatList[A]): Option[CatList[A]] = cl match {
    case CatNil => None
    case cc: CatCons[A] => None
  }

  def head[A](cl: CatList[A]): Option[A] = cl match {
    case CatNil => None
    case CatCons(x, _) => Some(x)
  }
}



