package com.ojha.scalazbook

import simulacrum.{op, typeclass}

object Chapter4Main {

  // value types
  case object Bow

  type Name = String
  type Age = Int

  // product types
  final case class Cat(name: Name, age: Age)

  // coproduct types
  sealed abstract class CatType

  case object Tortie extends CatType

  case object Tabby extends CatType

  // a generalised ADT has a type param
  // this list is also a recursive  type
  sealed abstract class CatList[T]

  case object CatNil extends CatList

  case class CatCons[T](t: T, tail: CatList[T]) extends CatList[T]

  def nonEmpty[T](cl: CatList[T]): Boolean = cl match {
    case CatCons(t, tail) if t.toString.equals("cat") => true
  }

}

@typeclass trait Ordering[T] {
  def compare(x: T, y: T): Int

  @op("<") def lt(x: T, y: T): Boolean = compare(x, y) < 0

  @op(">") def gt(x: T, y: T): Boolean = compare(x, y) > 0
}

@typeclass trait Numeric[T] extends Ordering[T] {
  @op("+") def plus(x: T, y: T): T

  @op("*") def times(x: T, y: T): T

  @op("unary_-") def negate(x: T): T

  def zero: T

  def abs(x: T): T = if (lt(x, zero)) negate(x) else x
}

object TypeClassesMain {

  import Numeric.ops._
  def signOfTheTimes[T: Numeric](t: T): T = -(t.abs) * t

  implicit val NumericDouble: Numeric[Double] = new Numeric[Double] {
    def plus(x: Double, y: Double): Double = x + y
    def times(x: Double, y: Double): Double = x * y
    def negate(x: Double): Double = -x
    def zero: Double = 0.0
    def compare(x: Double, y: Double): Int = java.lang.Double.compare(x, y)
    // optimised
    override def lt(x: Double, y: Double): Boolean = x < y
    override def gt(x: Double, y: Double): Boolean = x > y
    override def abs(x: Double): Double = java.lang.Math.abs(x)
  }

  import java.math.{BigDecimal => BD}
  implicit val NumericBD: Numeric[BD] = new Numeric[BD] {
    def plus(x: BD, y: BD): BD = x.add(y)
    def times(x: BD, y: BD): BD = x.multiply(y)
    def negate(x: BD): BD = x.negate
    def zero: BD = BD.ZERO
    def compare(x: BD, y: BD): Int = x.compareTo(y)
  }

  final case class Complex[T](r: T, i: T)

  def cat[T: Numeric]: Numeric[Complex[T]] = new Numeric[Complex[T]] {
    override def plus(x: Complex[T], y: Complex[T]): Complex[T] = ???

    override def times(x: Complex[T], y: Complex[T]): Complex[T] = ???

    override def negate(x: Complex[T]): Complex[T] = ???

    override def zero: Complex[T] = ???

    override def compare(x: Complex[T], y: Complex[T]): Int = ???
  }

}

object Implicitnezz {

  trait Funky[F[_]] {
    def chicken() = println("chicken")
  }

  implicit val funkyOption = new Funky[Option] {}

  def f1[F[_]](implicit funky: Funky[F]) = funky.chicken()

  def f2[F[_]: Funky] = implicitly[Funky[F]].chicken()

  object Funky {
    def apply[F[_]](implicit f: Funky[F]) = f
  }

  def f3[F[_]: Funky] = Funky[F].chicken()




}