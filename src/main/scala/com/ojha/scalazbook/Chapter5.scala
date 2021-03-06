package com.ojha.scalazbook


import scalaz.Scalaz._
import scalaz._
import TradeTemplate._

object AppendableThings extends App {

  //    def meow(templates: List[TradeTemplate]) = {
  //      val zero = Monoid[TradeTemplate].zero
  //      templates.foldLeft(zero)(_ |+| _)
  //    }

  val r = Option[Currency](EUR) |+| Option[Currency](USD)
  //  val r = Option(1) |+| Option(2)
  val meow = 'm' |-> 'u'
  println(meow)

}

sealed abstract class Currency

case object EUR extends Currency

case object USD extends Currency

final case class TradeTemplate(payments: List[java.time.LocalDate],
                               ccy: Option[Currency],
                               otc: Option[Boolean])

object TradeTemplate {
  implicit val monoid: Monoid[TradeTemplate] = Monoid.instance(
    (a, b) => TradeTemplate(a.payments |+| b.payments,
      a.ccy |+| b.ccy,
      a.otc |+| b.otc),
    TradeTemplate(Nil, None, None)
  )

  implicit def lastWins[A]: Monoid[Option[A]] = Monoid.instance(
    {
      case (None, None) => None
      case (only, None) => only
      case (None, only) => only
      case (_, winner) => winner
    },
    None
  )

}
