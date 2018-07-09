package com.ojha.scalazbook

import java.time.temporal.ChronoUnit

import scala.concurrent.duration._
import scalaz.Scalaz._
import scalaz._

object Chapter3Main extends App {

  import java.time.Instant

  import scalaz.NonEmptyList

  trait Drone[F[_]] {
    def getBacklog: F[Int]

    def getAgents: F[Int]
  }

  final case class MachineNode(id: String)

  trait Machines[F[_]] {
    def getTime: F[Instant]

    def getManaged: F[NonEmptyList[MachineNode]]

    def getAlive: F[Map[MachineNode, Instant]]

    def start(node: MachineNode): F[MachineNode]

    def stop(node: MachineNode): F[MachineNode]
  }

  final case class WorldView(backlog: Int,
                             agents: Int,
                             managed: NonEmptyList[MachineNode],
                             alive: Map[MachineNode, Instant],
                             pending: Map[MachineNode, Instant],
                             time: Instant)

  final class DynAgents[F[_]](D: Drone[F], M: Machines[F])(implicit X: Monad[F]) {
    var state = initial

    //    def go(): Unit = {
    //
    //
    //      while (true) {
    //        state = update(state)
    //        state = act(state)
    //      }
    //    }

    def initial: F[WorldView] = for {
      backlog <- D.getBacklog
      agents <- D.getAgents
      managed <- M.getManaged
      alive <- M.getAlive
      instant <- M.getTime
    } yield WorldView(backlog, agents, managed, alive, Map.empty, instant)


    def update(old: WorldView): F[WorldView] = for {
      snapshot <- initial
      changed = getDiff(old.alive.keySet, snapshot.alive.keySet)
      pending: Map[MachineNode, Instant] = old.pending -- changed
      update = snapshot.copy(pending = pending)
    } yield update

    def act(worldView: WorldView): F[WorldView] = worldView match {
      case NeedsAgent(node) => {
        for {
          _ <- M.start(node)
          update = worldView.copy(pending = Map(node -> worldView.time))
        } yield update
      }
      case Stale(nodes) => {

        nodes.foldLeftM(worldView) { (world, node) => {
          for {
            _ <- M.stop(node)
            update = world.copy(pending = world.pending + (node -> world.time))
          } yield update
        }
        }
      }
      case _ => worldView.pure[F]
    }

    private def getDiff[T](s1: Set[T], s2: Set[T]): Set[T] = (s1 union s2) -- (s1 intersect s2)


    private def symdiff[T](a: Set[T], b: Set[T]): Set[T] =
      (a union b) -- (a intersect b)

    private def timediff(from: Instant, to: Instant): FiniteDuration =
      ChronoUnit.MINUTES.between(from, to).minutes

  }

  private object NeedsAgent {
    def unapply(worldView: WorldView): Option[MachineNode] = worldView match {
      case WorldView(backlog, 0, managed, alive, pending, _)
        if backlog > 0 && alive.isEmpty && pending.isEmpty => Option(managed.head)
      case _ => None
    }
  }

  private object Stale {
    def unapply(worldView: WorldView): Option[NonEmptyList[MachineNode]] = worldView match {
      case WorldView(backlog, agents, managed, alive, pending, currentTime)
        if alive.nonEmpty => {
        (alive -- pending.keys)
          .filter(kv => {
            val node = kv._1
            val uptime = timediff(kv._2, currentTime)
            (backlog == 0 && uptime.toMinutes % 60 >= 58) || uptime.toHours >= 5
          }).keys.toList.toNel
      }
      case _ => None
    }

    private def timediff(from: Instant, to: Instant): FiniteDuration =
      ChronoUnit.MINUTES.between(from, to).minutes
  }

}
