package com.ojha

object Quicksort {

  def lomuto(arr: Array[Int]): Unit = aux(arr, 0, arr.length)

  private def aux(arr: Array[Int], l: Int, r: Int) = {

    ???
  }

  private def swap(arr: Array[Int], i: Int, j: Int) = {
    val ai = arr(i)
    arr(i) = arr(j)
    arr(j) = ai
  }

}
