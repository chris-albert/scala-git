package io.lbert.git

import java.nio.file.{Files, Path}
import cats.ApplicativeError
import scala.io.Source

trait File[F[_]] {
  def read(path: Path): F[List[String]]
  def bytes(path: Path): F[Array[Byte]]
  def exists(path: Path): F[Boolean]
}

object File {

  def apply[F[_]](implicit F: File[F]): File[F] = F

  implicit def FileF[F[_]](
    implicit
    A: ApplicativeError[F, Throwable]
  ): File[F] = new File[F] {
    override def read(path: Path): F[List[String]] =
      ApplicativeError[F, Throwable].catchNonFatal(
        Source.fromFile(path.toFile).getLines().toList
      )
    override def bytes(path: Path): F[Array[Byte]] =
      ApplicativeError[F, Throwable].catchNonFatal(
        Files.readAllBytes(path)
      )
    override def exists(path: Path): F[Boolean] =
      ApplicativeError[F, Throwable].catchNonFatal(
        Files.exists(path)
      )
  }
}
