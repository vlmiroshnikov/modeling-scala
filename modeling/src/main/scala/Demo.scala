package demo

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO

import reporting.*
import models.*
import pretty.*

object App extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    val stockholm  = Project.Leaf("Stockholm", ProjectId.of(1))
    val gothenburg = Project.Leaf("Gothenburg", ProjectId.of(2))
    val city       = Project.Leaf("Malmo City", ProjectId.of(3))
    val limhamn    = Project.Leaf("Limhamn", ProjectId.of(4))

    val malmo       = Project.Group("Malmo", ProjectId.of(6), List(city, limhamn))
    val someProject = Project.Group("Sweden", ProjectId.of(7), List(stockholm, gothenburg, malmo))

    for
      _   <- IO(draw(asTree(someProject)).foreach(println(_)))
      res <- calcProjectReport[IO](someProject)
      _   <- IO(draw(asTree(res)).foreach(println(_)))
    yield ExitCode.Success
}
