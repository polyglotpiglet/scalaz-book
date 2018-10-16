package com.ojha.red

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}

import com.google.common.util.concurrent.ThreadFactoryBuilder

import scala.collection.immutable

trait CatFuture[A] {
  def apply(k: A => Unit): Unit
}

object ParPlus {

  type Par[A] = ExecutorService => CatFuture[A]

  def unit[A](a: A): Par[A] = es => (k: A => Unit) => k(a)

  def fork[A](pa: => Par[A]): Par[A] =
    es => (cb: A => Unit) => eval(es)(pa(es)(cb))

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call: Unit = r
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](ex: ExecutorService)(pa: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    pa(ex)(a => {
      ref.set(a)
      latch.countDown()
    })
    latch.await()
    ref.get()
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = es => {
    val as: List[A] = ps.map(f => ParPlus.run(es)(f))
    unit(as)(es)
  }

}


object BadMySequenceParPlus extends App {

  import ParPlus._

  val namedThreadFactory = new ThreadFactoryBuilder()
    .setNameFormat("noodle-%d")
    .build()

  val es: ExecutorService = Executors.newFixedThreadPool(2, namedThreadFactory)

  val one = lazyUnit(get(1))
  val two  = lazyUnit(get(2))

  val sequenced = fork(fork(sequence(List(one, two))))

  val res: immutable.Seq[Int] = run(es)(sequenced)
  println(res)
  es.shutdown()

  def get(i: Int): Int = i

}
