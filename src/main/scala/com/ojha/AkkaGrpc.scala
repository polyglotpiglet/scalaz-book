package com.ojha

import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

trait AkkaGrpcClient {
  def closed(): Future[Unit]
  def close(): Future[Unit]
}

object RestartingClient {
  def apply[T <: AkkaGrpcClient](createClient: () => T)(implicit ec: ExecutionContext): RestartingClient[T] =
    new RestartingClient[T](createClient)
}

class RestartingClient[T <: AkkaGrpcClient](createClient: () => T)(implicit ec: ExecutionContext) {

  private val clientRef = new AtomicReference[T](create())

  def client(): T = clientRef.get()

  def withClient[A](f: T => A): A = {
    f(client())
  }

  def close(): Unit = {
    val c = client()
    c.close()

  }

  private def create(): T = {
    val c: T = createClient()
    c.closed().onComplete {
      case Success(_) =>
      case Failure(_) => clientRef.set(create())
    }
    c
  }
}

