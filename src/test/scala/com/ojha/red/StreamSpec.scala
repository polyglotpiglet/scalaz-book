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
      Stream(1, 2, 3, 4, 5).filter(_ => true).toList shouldEqual List(1, 2, 3, 4, 5)
    }

    "always returns empty for Empty stream" in {
      Empty.filter(_ => true) shouldBe Empty
      Empty.filter(_ => false) shouldBe Empty
    }
  }

  "append" must {
    "work for two non empty streams" in {
      Stream(1, 2, 3, 4, 5).append(Stream(6, 7, 8, 9)).toList shouldEqual List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    }

    "work where left is empty" in {
      Empty.append(Stream(1, 2, 3)).toList shouldEqual List(1, 2, 3)
    }

    "work where right is empty" in {
      Stream(1, 2, 3).append(Empty).toList shouldEqual List(1, 2, 3)
    }
  }

  "flatMap" must {
    "work for non empty stream" in {
      Stream(1, 2, 3, 4, 5).flatMap(x => Stream(x)).toList shouldEqual List(1, 2, 3, 4, 5)
    }

    "always returns empty for Empty stream" in {
      Empty.flatMap(x => Stream(x)) shouldBe Empty
      Empty.flatMap(x => Stream(x)) shouldBe Empty
    }

  }

  "ones" must {
    "have take" in {
      lazy val ones: Stream[Int] = Stream.cons(1, ones)
      ones.take(100).toList shouldEqual List.fill(100)(1)
    }
  }

  "constant" must {
    "have take" in {
      Stream.constant(5).take(100).toList shouldEqual List.fill(100)(5)
    }
  }

  "from" must {
    "have take" in {
      Stream.from(5).take(100).toList shouldEqual (5 until 105).toList
    }
  }

  "fibs" must {
    "have take" in {
      Stream.fibs.take(7).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8)
    }
  }

  "unfold" must {
    "can build list from 0 to 10" in {
      Stream.unfold[Int, Int](0)(s => if (s > 10) None else Some(s, s + 1)).toList shouldEqual (0 to 10).toList
    }
  }

  "zipwith" must {
    "can zip two intStreams" in {

      val first = Stream.from(0)
      val second = Stream.from(5)

      first.zipWith(second).take(5).toList shouldEqual List((0, 5), (1, 6), (2, 7), (3, 8), (4, 9))

      Stream.unfold[Int, Int](0)(s => if (s > 10) None else Some(s, s + 1)).toList shouldEqual (0 to 10).toList
    }
  }

  "zipAll" must {
    "can zip two streams with the same length" in {

      val first = Stream.from(0)
      val second = Stream.from(5)

      val firstPair = (Some(0), Some(5))
      val secondPair = (Some(1), Some(6))
      val thirdPair = (Some(2), Some(7))
      first.zipAll(second).take(3).toList shouldEqual List(firstPair, secondPair, thirdPair)
    }

    "can zip two streams where the left is empty" in {

      val first = Empty
      val second = Stream.from(5)
      val firstPair = (None, Some(5))
      val secondPair = (None, Some(6))
      val thirdPair = (None, Some(7))
      first.zipAll(second).take(3).toList shouldEqual List(firstPair, secondPair, thirdPair)
    }

    "can zip two streams where the right is empty" in {
      val first = Stream.from(5)
      val second = Empty
      val firstPair = (None, Some(5)).swap
      val secondPair = (None, Some(6)).swap
      val thirdPair = (None, Some(7)).swap
      first.zipAll(second).take(3).toList shouldEqual List(firstPair, secondPair, thirdPair)
    }
  }

  "startsWith" must {
    "is true and streams are the same length" in {
      Stream(1, 2, 3, 4).startsWith(Stream(1, 2, 3, 4)) shouldBe true
      Empty.startsWith(Empty) shouldBe true
    }

    "is false and left is shorter" in {
      Stream(1, 2).startsWith(Stream(1, 2, 3, 4)) shouldBe false
      Empty.startsWith(Stream(1, 2)) shouldBe false
    }

    "is true and left is longer" in {
      Stream(1, 2, 3, 4, 5).startsWith(Stream(1, 2, 3, 4)) shouldBe true
      Stream(1, 2).startsWith(Stream(1)) shouldBe true
    }
  }

  "tails" must {
    "can get all tails" in {
      val actual = Stream(1, 2, 3).tails.toList.map(_.toList)
      val expected = List(List(1, 2, 3), List(2, 3), List(3))
      actual shouldEqual expected

    }


  }

}
