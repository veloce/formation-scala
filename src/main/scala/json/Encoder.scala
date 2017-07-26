package json

trait Encoder[-T] {
  def encode(v: T): JValue
}

trait OEncoder[-T] extends Encoder[T] { self =>
  def encode(v: T): JObject

  def contramap[U](f: U => T) = new OEncoder[U] {
    def encode(v: U) = self.encode(f(v))
  }
}

object OEncoder {
  implicit val encoderBuilder = new Builder[OEncoder] {
    def apply[A, B](ea: OEncoder[A], eb: => OEncoder[B]): OEncoder[(A, B)] = new OEncoder[(A, B)] {
      def encode(v: (A, B)) = v match {
        case (a, b) => ea.encode(a).merge(eb.encode(b))
      }
    }
  }
}

object Encoder {
  def apply[A](f: A => JValue): Encoder[A] =
    new Encoder[A] { def encode(a: A) = f(a) }

  def fieldEncoder[T : Encoder](k: String): OEncoder[T] = new OEncoder[T] {
    def encode(v: T) = JObject(Map(k -> JValue.toJson(v)))
  }

  implicit val stringEncoder = new Encoder[String] {
    def encode(v: String) = JString(v)
  }

  implicit val intEncoder = new Encoder[Int] {
    def encode(v: Int) = JNumber(v.toDouble)
  }

  implicit def seqEncoder[T: Encoder] = new Encoder[Seq[T]] {
    def encode(v: Seq[T]) = JArray(v.map(JValue.toJson(_)))
  }

}
