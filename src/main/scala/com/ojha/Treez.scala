package com.ojha

trait Tree

case class Node[T](value: T, left: Tree = null, right: Tree = null) extends Tree

object Tree {


  def buildATree(): Unit = {
    val one = Node(1)
    val two = Node(2)
    val three = Node(3, two, one)
  }


}
