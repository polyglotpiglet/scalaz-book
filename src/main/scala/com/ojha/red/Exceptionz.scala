package com.ojha.red

import cats.effect.{Effect, IO}
import cats.implicits._


sealed trait CatOption[+A] {
  def map[B](f: A => B): CatOption[B] = this match {
    case CatSome(a) => CatSome(f(a))
    case _ => CatNone
  }

  def flatMap[B](f: A => CatOption[B]): CatOption[B] = map(f) getOrElse CatNone

  def filter(f: => A => Boolean): CatOption[A] = if (map(f) getOrElse false) this else CatNone

  def getOrElse[B >: A](default: => B): B = this match {
    case CatNone => default
    case CatSome(a) => a
  }

  def orElse[B >: A](default: => CatOption[B]): CatOption[B] = map(x => CatSome(x)) getOrElse default


}

object CatOption {
  def map2[A, B, C](a: CatOption[A], b: CatOption[B])(f: (A, B) => C): CatOption[C] =
    a flatMap (i => b.map(j => f(i, j)))

  def sequence[A](a: List[CatOption[A]]): CatOption[List[A]] = {
    val initial: CatOption[List[A]] = CatSome(List.empty[A])

    a.foldLeft(initial)((b, oA) => {
      (b, oA) match {
        case (CatSome(ls), CatSome(i)) => CatSome(ls :+ i)
        case _ => CatNone
      }
    })
  }


  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]]

}

case class CatSome[+A](get: A) extends CatOption[A]

case object CatNone extends CatOption[Nothing]

object CatMath {

  def mean(xs: Seq[Double]): CatOption[Double] = xs.length match {
    case 0 => CatNone
    case l => CatSome(xs.sum / l)
  }

  def variance(xs: Seq[Double]): CatOption[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
}

object Blah extends App {

  //  def nastyDatabaseCall(query: String): Future[Int] = {
  //    if (query.startsWith("a"))
  //      Future.failed(new RuntimeException("going and hoovering when i am working with you pisses me off"))
  //    else
  //      Future(42)
  //  }

  val meow: IO[Unit] = for {
    _ <- IO(println("What are your kitties called?"))
    names <- IO(io.StdIn.readLine)
    individualNames = names.split(" ").toList
    _ <- individualNames.traverse { name =>
      IO(println("I love " + name))
    }

  } yield ()

  meow.unsafeRunSync()

  //  val ares: IO[Int] = IO.async({ k: (Either[Throwable, Int] => Unit) =>
  //
  //    val x: Future[Int] =nastyDatabaseCall("apuff")
  //    x.onComplete {
  //      case Success(s) => k(s.asRight)
  //      case Failure(t) => k(t.asLeft)
  //    }
  //  })

  abstract class Terminal[F[_] : Effect] {
    def read(): F[String]

    def write(name: String): F[Unit]

    def echo(): F[Unit] = for {
      name <- read()
      _ <- write(name)
    } yield ()
  }


}
