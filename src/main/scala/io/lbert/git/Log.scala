package io.lbert.git

import cats.Applicative

trait Log[F[_]] {
  def log(s: String): F[Unit]
  def log(h: String, s: String): F[Unit]
  def log(h: String, b: Array[Byte]): F[Unit]
}

object Log {
  def apply[F[_]](implicit L: Log[F]): Log[F] = L

  implicit def LogF[F[_]: Applicative]: Log[F] = new Log[F] {
    override def log(s: String): F[Unit] =
      Applicative[F].pure(println(s))

    override def log(h: String, s: String): F[Unit] =
      log(s"$h:\n$s")

    override def log(h: String, b: Array[Byte]): F[Unit] =
      log(h, convertBytesToUTF8(b))
  }

  private def convertBytesToUTF8(bytes: Seq[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append(b.toChar)
    }
    sb.toString
  }
}

