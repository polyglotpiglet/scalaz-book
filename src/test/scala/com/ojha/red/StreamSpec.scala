package com.ojha.red

import org.scalatest.{Matchers, WordSpec}

class StreamSpec extends WordSpec with Matchers {


  "A stream headOption" must {
    "return None for Nil" in {
      Empty.headOption shouldEqual None
    }
    "return head for unit stream" in {
      Stream(1).headOption shouldEqual Some(1)
    }

    "return head for longer stream" in {
      Stream(1, 2, 3, 4).headOption shouldEqual Some(1)
    }

    //    "return head without evaluating tail" in {
    //      Stream(1, {throw new RuntimeException; 2}).headOption shouldEqual Some(1)
    //    }

    // how to make this fail???
    "return head twice only evaluates expression once" in {
      var i = 0
      val stream = Stream({
        i += 1;
        println("cat");
        1
      })
      stream.headOption shouldEqual stream.headOption
      i shouldBe 1
    }

  }
  "A stream toList" must {
    "return empty list for Empty" in {
      Empty.toList shouldEqual Nil
    }

    "return sensible list for non empty stream" in {
      Stream(1, 2, 3, 4, 5).toList shouldEqual List(1, 2, 3, 4, 5)
    }

  }

  "take(n)" must {
    "work for non empty stream" in {
      Stream(1, 2, 3, 4, 5).take(3).toList shouldEqual Stream(1, 2, 3).toList
      Stream(1, 2, 3, 4, 5).take(1).toList shouldEqual Stream(1).toList
      Stream(1, 2, 3, 4, 5).take(0) shouldEqual Empty
    }

    "take 0 from an empty stream" in {
      Empty.take(0) shouldEqual Empty
    }
  }

  "drop(n)" must {
    "work for non empty stream" in {
      Stream(1, 2, 3, 4, 5).drop(3).toList shouldEqual Stream(4, 5).toList
      Stream(1, 2, 3, 4, 5).drop(1).toList shouldEqual Stream(2, 3, 4, 5).toList
      Stream(1, 2, 3, 4, 5).drop(5) shouldEqual Empty
    }

    "drop 0 from an empty stream" in {
      Empty.drop(0) shouldEqual Empty
    }
  }

  "takeWhile(p: A => Boolean)" must {
    "work for non empty stream" in {
      Stream(1, 2, 3, 4, 5).takeWhile(_ < 4).toList shouldEqual Stream(1, 2, 3).toList
      Stream(1, 2, 3, 4, 5).takeWhile(_ > 0).toList shouldEqual Stream(1, 2, 3, 4, 5).toList
      Stream(1, 2, 3, 4, 5).takeWhile(_ => false) shouldEqual Empty
    }

    "always returns Empty for Empty stream" in {
      Empty.takeWhile(_ => true) shouldEqual Empty
      Empty.takeWhile(_ => false) shouldEqual Empty
    }
  }

  "forAll" must {
    "work for non empty stream" in {
      Stream(1, 2, 3, 4, 5).forAll(_ < 4) shouldBe false
      Stream(1, 2, 3, 4, 5).forAll(_ < 40) shouldBe true
    }

    "always returns true for Empty stream" in {
      Empty.forAll(_ => true) shouldBe true
      Empty.forAll(_ => false) shouldBe true
    }

  }
  "map" must {
    "work for non empty stream" in {
      Stream(1, 2, 3, 4, 5).map(_ * 2).toList shouldEqual List(2, 4, 6, 8, 10)
    }

    "always returns empty for Empty stream" in {
      Empty.map(identity) shouldBe Empty
    }
  }

  "filter" must {
    "work for non empty stream" in {
      Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList shouldEqual List(2, 4)
      Stream(1, 2, 3, 4, 5).filter(_ => false).toList shouldEqual List()
      Stream(1, 2, 3, 4, 5).filter(_ => true).toList shouldEqual List(1,2,3,4,5)
    }

    "always returns empty for Empty stream" in {
      Empty.filter(_ => true) shouldBe Empty
      Empty.filter(_ => false) shouldBe Empty
    }
  }

  "append" must {
    "work for two non empty streams" in {
      Stream(1, 2, 3, 4, 5).append(Stream(6,7,8,9)).toList shouldEqual List(1,2,3,4,5,6,7,8,9)
    }

    "work where left is empty" in {
    }
  }


}
