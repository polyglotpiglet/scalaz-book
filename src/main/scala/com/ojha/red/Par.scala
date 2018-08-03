package com.ojha.red

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}


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

    def get(timeout: Long, units: TimeUnit): (A,B) = {
      val timeoutNano = units.convert(timeout, TimeUnit.NANOSECONDS)

      val startNano = System.nanoTime()
      val a = f1.get(timeoutNano, TimeUnit.NANOSECONDS)
      val endNano = System.nanoTime()

      val timeToEvalA = endNano - startNano
      val b = f2.get(timeoutNano - timeToEvalA, TimeUnit.NANOSECONDS)

      (a,b)
    }

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }


  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = es => {
    val a = pa(es)
    val b = pb(es)
    UnitFuture(f(a.get, b.get))
  }


  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call: A = a(es).get
  })

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](ex: ExecutorService)(pa: Par[A]): Future[A] = pa(ex)

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = es => UnitFuture(ps.map(a => a(es).get()))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }



}
