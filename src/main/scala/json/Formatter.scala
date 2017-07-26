package json

trait Formatter[T] extends Encoder[T] with Decoder[T]

trait OFormatter[T] extends OEncoder[T] with Decoder[T] with Formatter[T] { self =>

  def inmap[B](f1: T => B, f2: B => T): OFormatter[B] = new OFormatter[B] {
    def decode(v: JValue) = self.decode(v).map(f1)
    def encode(b: B) = self.encode(f2(b))
  }
}

object OFormatter {

  implicit def formatterBuilder(implicit dcb: Builder[Decoder], ecb: Builder[OEncoder]) = new Builder[OFormatter] {
    def apply[A, B](fa: OFormatter[A], fb: => OFormatter[B]) =
      OFormatter[(A, B)](dcb(fa, fb), ecb(fa, fb))
  }

  def apply[A](d: Decoder[A], e: OEncoder[A]): OFormatter[A] = new OFormatter[A] {
    def decode(js: JValue): Result[A] = d.decode(js)

    def encode(a: A): JObject = e.encode(a)
  }
}

object Formatter {

  def apply[A](fjs: Decoder[A], tjs: Encoder[A]): Formatter[A] = new Formatter[A] {
    def decode(json: JValue) = fjs.decode(json)
    def encode(o: A) = tjs.encode(o)
  }

  def fieldFormatter[T](k: String)(implicit d: Decoder[T], e: Encoder[T]) = OFormatter(Decoder.fieldDecoder(k), Encoder.fieldEncoder(k))
}
