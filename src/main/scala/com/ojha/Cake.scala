package com.ojha

trait DataReader {
  def readName(): String
}

trait DummyDataReader extends DataReader {
  def readName(): String = "bEllA"
}

class GreeterService {
  self: DataReader =>

  def greet(): Unit = {
    val formattedName = self.readName().toLowerCase.capitalize
    println("Hi there, " + formattedName)
  }
}

object Main extends App {
  val greeter = new GreeterService() with DummyDataReader
  greeter.greet()

  // if you want to know why people hate the cake pattern then here is the answer:
  greeter.readName()

  // wtf?! It exposes all the methods of all the dependencies! Grim
}