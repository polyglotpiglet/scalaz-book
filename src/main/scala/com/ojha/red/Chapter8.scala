package com.ojha.red

object Chapter8 {

  /*
  8.1 - how to test the following function
           sum: List[Int] => Int

  - if (list.distinct.count == 1) { list.length * list[0] == sum ) <-- be wary of (-1, -1, -1) vs (1,1,1)
  - sum(list.tail) + list.head = sum(list)
  - sum(list.map(_ * 2)) = 2 * sum(list)
   */

  /*
  8.2 What properties specify a function that finds the maximum of a list?

  - max(list) = list.sorted.last
  - max(list) = list.sorted.reverse
  - val randomIndex = Random(0, list.length); list[randomIndex] <= max(list)

   */



  val intList: Gen[List[Int]] = Gen.listOf(Gen.choose(0,100))

  val prop: Prop =
    Gen.forAll(intList)(ns => ns.reverse.reverse == ns) &&
      Gen.forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)



}

trait Prop {
  def &&(p: Prop): Prop = if (p.check && this.check) PropSuccess else PropFailure

  def check: Boolean
}

object Prop {
  type SuccessCount = Int

}

case object PropFailure extends Prop {
  override def check = false
}

case object PropSuccess extends Prop {
  override def check = true
}


trait Gen[A] {
  def generate(): A
}

object Gen {
  def listOf[A](gen: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, gen: Gen[A]): Gen[List[A]] = ???

  def choose(lower: Int, upper: Int): Gen[Int] = ???

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???


}
