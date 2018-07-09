package com.ojha.scalazbook

import com.ojha.scalazbook.Cat.Now

import scala.concurrent.Future

trait Terminal[C[_]] {
  def read: C[String]

  def write(t: String): C[Unit]
}

object Cat {
  type Now[X] = X
}

object TerminalSync extends Terminal[Now] {
  def read: String = io.StdIn.readLine()

  def write(t: String): Unit = println(t)
}

object TerminalAsync extends Terminal[Future] {

  import scala.concurrent.ExecutionContext.Implicits.global

  def read: Future[String] = Future(io.StdIn.readLine())

  def write(t: String): Future[Unit] = Future(println(t))
}

trait Execution[C[_]] {
  def map[A, B](c: C[A])(f: A => B): C[B] = flatMap(c)(a => pure(f(a)))

  def flatMap[A, B](c: C[A])(f: A => C[B]): C[B]

  def pure[A](a: A): C[A]
}

object Echo {
  def echo[C[_]](implicit ex: Execution[C], terminal: Terminal[C]): C[Unit] = {
    val read =  terminal.read
    val mapped = ex.map(read)(s => "Hello " + s)
    ex.flatMap(mapped)(terminal.write)
  }
}


case class IO[A](a: () => A) {
  def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(a()).a())
}

object Main extends App {

//  implicit val nowExecution: Execution[Now] = new Execution[Now] {
//    override def flatMap[A, B](c: A)(f: A => B): B = f(c)
//    override def pure[A](a: A): A = a
//  }
//
//  implicit val syncTerminal: TerminalSync.type = TerminalSync

//  import ExecutionContext.Implicits.global
//
//  implicit val futureExecution: Execution[Future] = new Execution[Future] {
//    override def flatMap[A, B](c: Future[A])(f: A => Future[B]): Future[B] = c.flatMap(f)
//
//    override def pure[A](a: A): Future[A] = Future(a)
//  }
//  implicit val asyncTerminal: TerminalAsync.type = TerminalAsync

  implicit val ioExecution: Execution[IO] = new Execution[IO] {
    override def flatMap[A, B](c: IO[A])(f: A => IO[B]): IO[B] = c.flatMap(f)

    override def pure[A](a: A): IO[A] = IO(() => a)
  }

  implicit val ioTerminal: Terminal[IO] = new Terminal[IO] {
    override def read: IO[String] = IO(() => io.StdIn.readLine())

    override def write(t: String): IO[Unit] = IO(() => println(t))
  }

  import Echo._

  val result = echo
  result.a()

}

