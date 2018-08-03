package com.ojha.red

import com.ojha.red.Par.Par
import org.scalatest.{Matchers, WordSpec}

class ParSpec extends WordSpec with Matchers {

  "par" must {
    "initialize lazily" in {
      var calls = 0

      def list(): List[Int] = {
        calls += 1
        List(1, 2, 3)
      }

      val sut: Par[List[Int]] = Par.lazyUnit(list())

      calls shouldBe 0
    }

    "sequence lazily" in {
      var calls = 0

      def int(): Int = {
        calls += 1
        5
      }

      val sut: List[Par[Int]] = List(Par.lazyUnit(int()), Par.lazyUnit(int()))
      val sequenced: Par[List[Int]] = Par.sequence(sut)

      calls shouldBe 0
    }
  }


}
