package io.lbert.git

import java.nio.file.Paths
import cats.effect.{ExitCode, IO, IOApp}

object Playground extends IOApp {

  val gitDir = Paths.get("/Users/chrisalbert/git/haskell-alfred-github")
  val hash = Git.Hash("6b8eeaf1fae22a1cab89bb326f2346ec12239bae")

  implicit val GitF = Git.forRepo[IO](gitDir)

  override def run(args: List[String]): IO[ExitCode] = for {
    bs    <- Git[IO].readRaw(hash)
    _     <- Log[IO].log("Decompressed", bs)
  } yield ExitCode.Success
}
