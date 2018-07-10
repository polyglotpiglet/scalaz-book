package com.ojha.red

import org.scalatest.{Matchers, WordSpec}

class CatEitherSpec extends WordSpec with Matchers {

  "catEither" must {
    "map" in {
      CatRight(2).map(e => e * 2) shouldEqual CatRight(4)
      CatLeft(2).map(e => e.toString) shouldEqual CatLeft(2)
    }

    "orElse" in {
      CatRight(2).orElse(CatRight(3)) shouldEqual CatRight(2)
      CatLeft(2).orElse(CatRight(3)) shouldEqual CatRight(3)
      CatLeft(2).orElse(CatLeft(3)) shouldEqual CatLeft(3)
    }

    "map2" in {
      CatRight(2).map2(CatRight(3))(_ + _ ) shouldEqual CatRight(5)
      CatRight(2).map2(CatLeft("bad"))(_ + _ ) shouldEqual CatLeft("bad")
    }
  }


}
