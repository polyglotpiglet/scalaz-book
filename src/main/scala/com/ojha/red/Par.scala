package com.ojha.red

import java.util.concurrent._


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
    val pars: List[Par[List[A]]] = as.map(i => asyncF(a => if (f(a)) List(a) else List())(i))
    val ps: Par[List[List[A]]] = sequence(pars)
    map(ps)(_.flatten)
  }

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
  
}