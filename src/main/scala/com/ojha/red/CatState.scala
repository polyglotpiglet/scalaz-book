package com.ojha.red

import com.ojha.red.Randy._

trait RNG {
  def nextInt: (Int, RNG)
}



case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {


  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (i, state) if i < 0 => (-(i + 1), state)
      case (i, state) => (i, state)
    }
  }

  def double: Rand[Double] = map(_.nextInt)(i => i.toDouble / Integer.MAX_VALUE)


  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, state) = rng.nextInt
    val (d, statePrime) = double(state)
    ((i, d), statePrime)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (rnd, state) = intDouble(rng)
    (rnd.swap, state)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val initial = (List.empty[Int], rng)
    (0 until count).foldLeft(initial)((current, _) => {
      val (ls, state) = current
      val (next, updatedState) = state.nextInt
      (ls :+ next, updatedState)
    })
  }
}

object Randy {
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_,_))

  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rnd => {
    val (a, stateA) = ra(rnd)
    val (b, stateB) = rb(stateA)
    (f(a,b), stateB)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a,b))))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.foldLeft((List.empty[A], rng)) ((current, randA) => {
      val (ls, state1) = current
      val (a, state2) = randA(state1)
      (ls :+ a, state2)
    })
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, s1) = f(rng)
      g(a)(s1)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i => unit(i % n) }





//  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
//    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
}

