package reporting

import mocks.*
import models.*
import cats.*
import cats.syntax.all.*
import cats.data.*

case class Report(bugetProfit: Money, netProfit: Money, difference: Money)

object Report:

  given Show[Report] =
    Show.show(r => s"Profit = ${r.bugetProfit}, Net = ${r.netProfit}, Diff = ${r.difference}")

given Monoid[Report] = new Monoid[Report] {
  def empty: Report = Report(Money.zero, Money.zero, Money.zero)

  override def combine(x: Report, y: Report): Report =
    Report(x.bugetProfit + y.bugetProfit, x.netProfit + y.netProfit, x.difference + y.difference)
}

def calculateReport(b: Budget, trx: List[Transaction]): Report =
  val netProfit = trx.foldLeft(Money.of(0.0)) {
    case (agg, Transaction.Sale(v))     => agg + v
    case (agg, Transaction.Purchase(v)) => agg - v
  }
  val bugetProfit = b.income - b.expenditure
  Report(bugetProfit, netProfit, difference = bugetProfit - netProfit)

def calcProjectReport[F[_]: Applicative](
    p: Project[ProjectId]): F[Project[Report]] =
  def calc(p: Project[ProjectId]): WriterT[F, Report, Project[Report]] =
    p match
      case Project.Leaf(name, pid) =>
        WriterT {
          (MockDb.getBudget[F](pid), MockDb.getTransactions[F](pid))
            .mapN((b, trx) => calculateReport(b, trx))
            .map(report => (report, Project.Leaf(name, report)))
        }
      case Project.Group(name, _, projects) =>
        projects
          .traverse(calc)
          .listen
          .map((projects, report) => Project.Group(name, report, projects))

  calc(p).run.map(_._2)
