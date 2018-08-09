package com.ojha.red

import java.util.concurrent._

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


  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call: A = a(es).get
  })

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](ex: ExecutorService)(pa: Par[A]): Future[A] = pa(ex)

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = es => UnitFuture(ps.map(a => a(es)).map(_.get))

  def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

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

object ParSequence extends App {
  val es: ExecutorService = Executors.newFixedThreadPool(2)

  val one = Par.fork(Par.unit(get(1)))
  val two = Par.fork(Par.unit(get(2)))

  // mine
  val sequenced = Par.sequence(List(one, two))

  // theirs
  // val sequenced = Par.sequence_simple(List(one, two))
  val res = Par.run(es)(sequenced)
  println(res.get)

  def get(i: Int): Int = {
    println("Getting " + i)
    Thread.sleep(5000)
    i
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
  println(Par.equal(es)(a, fork(a)))

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

