package com.ojha

import java.time.Instant
import java.time.temporal.TemporalAccessor

import org.scalatest.FlatSpec
import scalaz.NonEmptyList
import scalaz.Scalaz.Id
import Data._
import com.ojha.Chapter3Main._
import org.scalatest.Matchers._

class Chapter3Spec extends FlatSpec {

  "Business Logic" should "generate an initial world view" in {
    val mutable = new Mutable(needsAgents)
    import mutable._
    program.initial shouldBe needsAgents
  }

}



object Data {

  import com.ojha.Chapter3Main._
  val node1 = MachineNode("1243d1af-828f-4ba3-9fc0-a19d86852b5a")
  val node2 = MachineNode("550c4943-229e-47b0-b6be-3d686c5f013f")
  val managed = NonEmptyList(node1, node2)
  import Instant.parse


  val time1: Instant = parse("2017-03-03T18:07:00.000+01:00[Europe/London]")
  val time2: Instant = parse("2017-03-03T18:59:00.000+01:00[Europe/London]") // +52 mins
  val time3: Instant = parse("2017-03-03T19:06:00.000+01:00[Europe/London]") // +59 mins
  val time4: Instant = parse("2017-03-03T23:07:00.000+01:00[Europe/London]") // +5 hours
  val needsAgents = WorldView(5, 0, managed, Map.empty, Map.empty, time1)
}

class Mutable(state: WorldView) {

  var started, stopped: Int = 0
  private val D: Drone[Id] = new Drone[Id] {
    def getBacklog: Int = state.backlog
    def getAgents: Int = state.agents
  }
  private val M: Machines[Id] = new Machines[Id] {
    def getAlive: Map[MachineNode, Instant] = state.alive
    def getManaged: NonEmptyList[MachineNode] = state.managed
    def getTime: Instant = state.time
    def start(node: MachineNode): MachineNode = { started += 1 ; node }
    def stop(node: MachineNode): MachineNode = { stopped += 1 ; node }
  }
  val program = new DynAgents[Id](D, M)
}