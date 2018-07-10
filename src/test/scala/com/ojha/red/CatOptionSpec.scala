package com.ojha.red

import org.scalatest.{Matchers, WordSpec}

class CatOptionSpec extends WordSpec with Matchers {

  "map" must {
    "work" in {
      CatSome(2).map(_ * 2) shouldEqual CatSome(4)
      CatNone.map(_.toString) shouldEqual CatNone
    }
  }

  "flat map" must {
    "work" in {
      CatSome(2).flatMap(i => CatSome(i * 2)) shouldEqual CatSome(4)
      CatNone.map(_.toString) shouldEqual CatNone
    }
  }

  "filter" must {
    "work" in {
      CatSome(2).filter(_ == 2) shouldEqual CatSome(2)
      CatSome(2).filter(_ > 2) shouldEqual CatNone
      CatNone.filter(_ => true) shouldEqual CatNone
    }
  }

  "getOrElse" must {
    "work" in {
      CatSome(2).getOrElse(3) shouldEqual 2
      CatNone.getOrElse(3) shouldEqual 3
    }
  }

  "orElse" must {
    "work" in {
      CatSome(2).orElse(CatSome(3)) shouldEqual CatSome(2)
      CatNone.orElse(CatNone) shouldEqual CatNone
      CatNone.orElse(CatSome(5)) shouldEqual CatSome(5)

    }
  }

  "cat option object" must {
    "have map 2" in {
      CatOption.map2(CatSome(1), CatSome(2))(_ + _) shouldEqual CatSome(3)
      CatOption.map2[Int, Int, Int](CatSome(1), CatNone)(_ + _) shouldEqual CatNone
      CatOption.map2[Int, Int, Int](CatNone, CatSome(3))(_ + _) shouldEqual CatNone
      CatOption.map2[Int, Int, Int](CatNone, CatNone)(_ + _) shouldEqual CatNone
    }

    "have sequence" in {
      CatOption.sequence(List(CatSome(1))) shouldEqual CatSome(List(1))
      CatOption.sequence(List(CatNone)) shouldEqual CatNone

      CatOption.sequence(List(CatSome(1), CatSome(2))) shouldEqual CatSome(List(1,2))
      CatOption.sequence(List(CatNone, CatSome(2))) shouldEqual CatNone
    }

    "have traverse" in {

      CatOption.traverse(List(1,2,3))(x => CatSome(x)) shouldEqual CatSome(List(1,2,3))
      CatOption.traverse(List(1,2,3))(x => CatNone) shouldEqual CatNone

    }
  }



}
