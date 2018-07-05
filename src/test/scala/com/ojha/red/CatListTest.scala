package com.ojha.red

import org.scalatest.{Matchers, WordSpec}
class CatListTest extends WordSpec with Matchers {

  "A list head" must  {
    "return None for Nil" in {
      CatList.head(CatNil) shouldEqual None
    }
    "return Head for non empty" in {
      CatList.head(CatCons(1, CatNil)) shouldEqual Option(1)
    }

    "return head without evaluating tail" in {
      val makeMeLazy = CatCons(1, { throw new RuntimeException(); CatNil})
      CatList.head(makeMeLazy) shouldEqual Option(1)
    }

  }

  "A list tail" must  {
    "return None for Nil" in {
      CatList.tail(CatNil) shouldEqual None
    }
    "return CatNil for unit list" in {
      CatList.tail(CatCons(1, CatNil)) shouldEqual Option(CatNil)
    }
    "return tail for longer list" in {
      CatList.tail(CatCons(1, CatCons(2, CatNil))) shouldEqual Option(CatCons(2, CatNil))
    }
//    "return head without evaluating tail" in {
//      val makeMeLazy = CatCons(1, { throw new RuntimeException(); CatNil})
//      CatList.head(makeMeLazy) shouldEqual Option(1)
//    }

  }

}
