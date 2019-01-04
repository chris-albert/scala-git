package io.lbert.git

import cats.ApplicativeError
import java.util.zip.{Inflater, Deflater}

trait Compression[F[_]] {
  def compress(data: Array[Byte]): F[Array[Byte]]
  def decompress(data: Array[Byte]): F[Array[Byte]]
}

object Compression {

  def apply[F[_]](implicit C: Compression[F]): Compression[F] = C

  /**
    * TODO: This is dumb, copied from internet.... Not functional at all, but it
    * is wrapped in a ApplicativeError... so i did that much :)
    */
  implicit def CompressionF[
    F[_] :
    ApplicativeError[?[_], Throwable]
  ]: Compression[F] = new Compression[F] {
    override def compress(data: Array[Byte]): F[Array[Byte]] =
      ApplicativeError[F, Throwable].catchNonFatal{
        var deflater: Deflater = new Deflater()
        deflater.setInput(data)
        deflater.finish
        val compressedData = new Array[Byte](data.size * 2) // compressed data can be larger than original data
        val count: Int = deflater.deflate(compressedData)
        compressedData.take(count)
      }

    override def decompress(data: Array[Byte]): F[Array[Byte]] =
    ApplicativeError[F, Throwable].catchNonFatal{
      val inflater = new Inflater()
      inflater.setInput(data)
      val decompressedData = new Array[Byte](data.size * 2)
      var count = inflater.inflate(decompressedData)
      var finalData = decompressedData.take(count)
      while (count > 0) {
        count = inflater.inflate(decompressedData)
        finalData = finalData ++ decompressedData.take(count)
      }
      decompressedData
    }
  }
}
