package com.ojha.red

sealed trait CatEither[+E, +A] {
  def map[B](f: A => B): CatEither[E, B] = this.flatMap(a => CatRight(f(a)))


  def flatMap[EE >: E, B](f: A => CatEither[EE, B]): CatEither[EE, B] = this match {
    case CatRight(a) => f(a)
    case CatLeft(e) => CatLeft(e)
  }

  def orElse[EE >: E, B >: A](b: => CatEither[EE, B]): CatEither[EE, B] = this match {
    case CatRight(a) => CatRight(a)
    case CatLeft(e) => b
  }

  def map2[EE >: E, B, C](b: CatEither[EE, B])(f: (A, B) => C):
  CatEither[EE, C] = for {
    aa <- this
    bb <- b
  } yield f(aa, bb)
}

case class CatLeft[+E](value: E) extends CatEither[E, Nothing]

case class CatRight[+A](value: A) extends CatEither[Nothing, A]
