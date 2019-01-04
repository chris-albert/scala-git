package io.lbert.git

import java.nio.file.{Path, Paths}
import cats.{Applicative, ApplicativeError, MonadError}
import cats.implicits._

trait Git[F[_]] {
  def whichObject(hash: Git.Hash): F[Git.Object]
  def getPath(hash: Git.Hash): F[Path]
  def readPath(hash: Git.Hash): F[Git.ObjectWithData]
  def readRaw(hash: Git.Hash): F[Array[Byte]]
}

object Git {

  val GIT_DIR    = Paths.get(".git")
  val OBJECT_DIR = GIT_DIR.resolve("objects")

  def apply[F[_]](implicit G: Git[F]): Git[F] = G

  def forRepo[
    F[_] :
    MonadError[?[_], Throwable]
  ](gitBasePath: Path): Git[F] = new Git[F] {

    override def readRaw(hash: Hash): F[Array[Byte]] = for {
      path  <- getPath(hash)
      bytes <- File[F].bytes(path)
      dec   <- Compression[F].decompress(bytes)
    } yield dec

    override def readPath(hash: Hash): F[ObjectWithData] = ???

    override def whichObject(hash: Hash): F[Object] = for {
      bytes <- readRaw(hash)
      out   <- Object.determine(bytes).liftTo[F](InvalidObject(bytes))
    } yield out

    override def getPath(hash: Git.Hash): F[Path] = for {
      path   <- getPathFromHash(hash)
      exists <- File[F].exists(path)
      _      <- if(exists) Applicative[F].unit
                else HashNotFound(hash).raiseError[F, Throwable]
    } yield path

    private def getPathFromHash(hash: Git.Hash): F[Path] =
      ApplicativeError[F, Throwable].catchNonFatal(
        gitBasePath.resolve(OBJECT_DIR)
          .resolve(s"${hash.hash.take(2)}/${hash.hash.drop(2)}")
      )
  }

  case class ObjectWithData(obj: Object, data: Array[Byte])

  case class HashNotFound(hash: Hash)
    extends Throwable(s"Hash not found [${hash.hash}]")

  case class InvalidObject(bytes: Array[Byte])
    extends Throwable(s"Unable parse object [${new String(bytes.take(6))}...]")

  sealed trait Object

  object Object {
    case object Blob extends Object
    case object Tree extends Object
    case object Commit extends Object
    case object Tag extends Object

    def determine(bytes: Array[Byte]): Option[Object] = {
      val head = new String(bytes.take(6))
      if(head.length == 6) {
        if(head.startsWith("commit"))    Some(Commit)
        else if(head.startsWith("tree")) Some(Tree)
        else if(head.startsWith("tag"))  Some(Tag)
        else if(head.startsWith("blob")) Some(Blob)
        else None
      } else None
    }
  }

  case class Hash(hash: String) extends AnyVal
}




