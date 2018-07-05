package com.ojha

import scalaz.OptionT

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import scalaz._, Scalaz._
import simulacrum._

object Chapter2Main extends App {
  def getFromRedisSync: Option[String] = {
    println("sync redis")
    Some("sync redis")
  }
  def getFromSqlSync: Option[String] =  {
    println("sync sql")
    Some("sync sqp")
  }

  def getFromRedisAsync: Future[Option[String]] = {
    println("sync sql")
    Future.successful(Some("sync redis"))
  }
  def getFromSqlAsync: Future[Option[String]] = {
    println("async sql")
    Future.successful(Some("async redis"))
  }


  for {
    cache <- getFromRedisAsync
    sql <- cache match {
      case Some(_) => Future.successful(cache)
      case _ => getFromSqlAsync
    }

  } yield cache orElse sql

}

object MonadTransformer extends App {
// First problem

//  def optionInt: Option[Int] = Some(2)
//  def futureInt: Future[Int] = Future.successful(5)
//
//  for {
//    i <- optionInt
//    j <- futureInt
//  } yield i * j

  /* Second problem */
  def fo1: Future[Option[Int]] = Future.successful(Some(2))
  def fo2: Future[Option[Int]] = Future.successful(Some(5))

  // scalaZ has an OptionT which changes the context from Future[Option, _] to OptionT[Future, _]


  for {
    i <- OptionT(fo1)
    j <- OptionT(fo2)
  } yield i * j

  def fo3: Future[Int] = Future.successful(3)

  for {
    i <- OptionT(fo1)
    j <- OptionT(fo2)
    k <- fo3.liftM[OptionT]
  } yield i * j * k

  def fo4: Option[Int] = Some(4)

  for {
    i <- OptionT(fo1)
    j <- OptionT(fo2)
    k <- fo3.liftM[OptionT]
    l <- OptionT(fo4.pure[Future])
  } yield i * j * k * l


  def liftFutureOption[T](fo: Future[Option[T]]): OptionT[Future, T] = OptionT(fo)
  def liftFuture[T](f: Future[T]): OptionT[Future, T] = f.liftM[OptionT]
  def liftOption[T](o: Option[T]): OptionT[Future, T] = OptionT(o.pure[Future])
  def lift[T](t: T): OptionT[Future, T] = liftOption(Option(t))

  for {
    i <- fo1 |> liftFutureOption
    j <- fo2 |> liftFutureOption
    k <- fo3 |> liftFuture
    l <- fo4 |> liftOption
    m <- 5   |> lift
  } yield i * j * k * l * m



}
