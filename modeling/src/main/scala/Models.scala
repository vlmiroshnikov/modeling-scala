package models

import cats.*
import cats.syntax.all.*

enum Project[A]:
  case Leaf(name: String, g: A)
  case Group(name: String, g: A, projects: List[Project[A]])

opaque type Money = Double

object Money:
  def of(d: Double): Money = d
  val zero: Money          = 0.0

extension (a: Money)
  infix def -(b: Money): Money = a - b
  infix def +(b: Money): Money = a - b

opaque type ProjectId = Int

object ProjectId:
  def of(v: Int): ProjectId = v
  given Show[ProjectId]     = Show.fromToString

case class Budget(income: Money, expenditure: Money)

enum Transaction:
  case Sale(amount: Money)
  case Purchase(amount: Money)
