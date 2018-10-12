package com.ojha.red

import java.util.concurrent._

import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.ojha.red.Par.Par


object Par {

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  private case class TimeoutFuture[A, B](f1: Future[A], f2: Future[B]) extends Future[(A, B)] {
    def isDone = true

    def get(): (A, B) = (f1.get(), f2.get())

    def get(timeout: Long, units: TimeUnit): (A, B) = {
      val timeoutNano = units.convert(timeout, TimeUnit.NANOSECONDS)

      val startNano = System.nanoTime()
      val a = f1.get(timeoutNano, TimeUnit.NANOSECONDS)
      val endNano = System.nanoTime()

      val timeToEvalA = endNano - startNano
      val b = f2.get(timeoutNano - timeToEvalA, TimeUnit.NANOSECONDS)

      (a, b)
    }

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }


  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = es => {
    val a: Future[A] = pa(es)
    val b: Future[B] = pb(es)
    UnitFuture(f(a.get, b.get))
  }

  def delay[A](a: => Par[A]): Par[A] = es => a(es)

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call: A = a(es).get
  })

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](ex: ExecutorService)(pa: Par[A]): Future[A] = pa(ex)

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = es => UnitFuture(ps.map(a => a(es)).map(_.get))

  def sequence2[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

//  def sequenceBalanced[A](l: List[Par[A]]): Par[List[A]] = {
//
//  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = as.map(asyncF((a: A) => if (f(a)) List(a) else List()))
    val ps: Par[List[List[A]]] = sequence(pars)
    map(ps)(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

}


object ParDifferentSequences extends App {
  val namedThreadFactory = new ThreadFactoryBuilder()
    .setNameFormat("es-thread-%d")
    .build()

  val es: ExecutorService = Executors.newFixedThreadPool(2, namedThreadFactory)

  val one = Par.lazyUnit(get(1))
  val two  = Par.lazyUnit(get(2))
  val three  = Par.lazyUnit(get(3))
  val four  = Par.lazyUnit(get(4))
  val five  = Par.lazyUnit(get(5))
  val six  = Par.lazyUnit(get(6))

  val sequenced = Par.sequence2(List(one, two, three, four, five, six))

  val res = Par.run(es)(sequenced)
  println(res.get)

  def get(i: Int): Int = {
    val thread = Thread.currentThread().getName
    val time1 = System.currentTimeMillis() / 100 % 1000
    println(s"$thread | $time1 | Starting | $i ")
    Thread.sleep(i * 3000)
    val time2 = System.currentTimeMillis() / 100 %1000
    println(s"$thread | $time2 | Finishing | $i ")
    println("-----------------------------------------------")
    i
  }

  es.shutdown()
}


object ParSequence extends App {
  val namedThreadFactory = new ThreadFactoryBuilder()
    .setNameFormat("es-thread-%d")
    .build()

  val es: ExecutorService = Executors.newFixedThreadPool(2, namedThreadFactory)

//  val one = Par.fork(Par.unit(get(1)))
//  val two = Par.fork(Par.unit(get(2)))

  val one = Par.lazyUnit(get(1))
  val two  = Par.lazyUnit(get(2))

//  val one = Par.unit(get(1))
//  val two  = Par.unit(get(2))

  val sequenced = Par.sequence(List(one, two))

  val res = Par.run(es)(sequenced)
  println(res.get)

  def get(i: Int): Int = {
    val thread = Thread.currentThread().getName;
    println(thread + ": Getting " + i)
    Thread.sleep(5000)
    i
  }
}

object Doodle extends App {

  def sum(is: List[Int]): Par[Int] =  is match {
    case Nil => Par.unit(0)
    case x::Nil => Par.lazyUnit(x)
    case _ => {
      val (l, r) =  is.splitAt(is.size / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }

  }

}


object Paragraphs extends App {
  val es: ExecutorService = Executors.newFixedThreadPool(2)

  val paragraph1 = "I am a cat. You are a dog."
  val paragraph2 = "I love cats. Ruby and Bella rock."

  val input = List(paragraph1, paragraph2)

  val parInput: Par[List[Int]] = Par.parMap(input)(par => par.split(" ").length)
  val result: Future[List[Int]] = parInput(es)
  println(result.get())
}

object Deadlock extends App {
  import Par._
  val a = lazyUnit(42 + 1)
  val es = Executors.newFixedThreadPool(1, new CatThreadFactory())
  val areEqual: Boolean = Par.equal(es)(a, fork(a))
  println(areEqual)

}


import java.util.concurrent.ThreadFactory

class CatThreadFactory extends ThreadFactory {
  override def newThread(r: Runnable) = new Thread(r, "Meow")
}

/*

 map(y)(id) == y

 map(y)(g) = g(y) = x  (1)

 map(x)(f) = f(x)
 map(map(y)(g))(f) = f(g(y))

 (f compose g) = h

 map(map(y)(g))(f) = h(y)

      (1)
 map(map(y)(g))(f) = map(y)(h)



 map(map(y)(g))(f) == map(y)(f compose g)



 */

