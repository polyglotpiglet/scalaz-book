package com.ojha.red

import com.ojha.red.Randy.Rand
import org.scalatest.{Matchers, WordSpec}

class CatStateSpec extends WordSpec with Matchers{

  private val rng = SimpleRNG(0)

  "rng" must {
    "get non negative int" in {
      RNG.nonNegativeInt(rng)._1 should be >= 0
    }

    "get a double" in {
      RNG.double(rng)._1 should be >= 0.0
      RNG.double(rng)._1 should be <= 1.0
    }

    "get an int and double" in {

      val ((i, d), state) = RNG.intDouble(rng)
      val (i2, statePrime) = rng.nextInt
      val (d2, _) = RNG.double(statePrime)

      i shouldEqual i2
      d shouldEqual d2
    }

    "get a list of ints" in {
      val (ns, state) = RNG.ints(5)(rng)
      ns.distinct.size should be > 1
      ns.size shouldBe 5
    }
  }

  "rand" must {
    "map" in {
      val expected = rng.nextInt._1
      Randy.map(rng => rng.nextInt)(_.toString)(rng)._1 shouldBe expected.toString
    }
    "sequence" in {
      val (v1, state1) = rng.nextInt
      val (v2, state2) = state1.nextInt
      val rand: Rand[Int] = rng => rng.nextInt

      val actual: (List[Int], RNG) = Randy.sequence(List(rand, rand))(rng)
      actual._1 shouldBe List(v1, v2)
    }
  }

}
