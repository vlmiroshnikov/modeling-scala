package mocks

import models.*

import scala.util.Random
import cats.*
import cats.syntax.all.*

object MockDb:

  def randomMoney(from: Double, to: Double): Money =
    Money.of(Random.between(from, to))

  def getBudget[F[_]: Applicative](id: ProjectId): F[Budget] =
    Budget(randomMoney(0, 4000), randomMoney(0, 4000)).pure[F]

  def getTransactions[F[_]: Applicative](p: ProjectId): F[List[Transaction]] =
    List(Transaction.Sale(randomMoney(0, 100)), Transaction.Purchase(randomMoney(0, 100))).pure[F]
