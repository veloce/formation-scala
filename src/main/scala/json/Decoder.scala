package json

trait Decoder[T] { self =>
  def decode(v: JValue): Result[T]

  def map[U](f: T => U): Decoder[U] = new Decoder[U] {
    def decode(v: JValue) = self.decode(v).map(f)
  }
}

object Decoder {
  def apply[A](f: JValue => Result[A]): Decoder[A] =
    new Decoder[A] { def decode(json: JValue) = f(json) }

  def fieldDecoder[T](k: String)(implicit dec: Decoder[T]) = new Decoder[T] {
    def decode(v: JValue) = v match {
      case JObject(fields) => fields.get(k) match {
        case Some(v) => v.validate[T]
        case None => Failure(Seq(s"field $k does not exist"))
      }

      case _ => Failure(Seq("Not an object"))
    }
  }

  implicit val decoderBuilder = new Builder[Decoder] {
    def apply[A, B](da: Decoder[A], db: => Decoder[B]): Decoder[(A, B)] = new Decoder[(A, B)] {
      def decode(v: JValue) = (da.decode(v), db.decode(v)) match {
        case (Success(a), Success(b)) => Success((a, b))
        case _ => Failure(Seq("does not work"))
      }
    }
  }

  implicit val stringDecoder = new Decoder[String] {
    def decode(v: JValue) = v match {
      case JString(s) => Success(s)
      case _ => Failure(Seq("Not a JString"))
    }
  }

  implicit val intDecoder = new Decoder[Int] {
    def decode(v: JValue) = v match {
      case JNumber(n) if n.isValidInt => Success(n.toInt)
      case _ => Failure(Seq("Not a JNumber int"))
    }
  }

  implicit def seqDecoder[T](implicit dec: Decoder[T]) = new Decoder[Seq[T]] {
    def decode(v: JValue) = v match {
      case JArray(els) => {
        val decoded = els.map(_.validate[T])
        val valids = decoded.collect { case Success(v) => v }
        val fails = decoded.collect { case Failure(er) => er }
        if (valids.size == els.size) Success(valids)
        else Failure(fails.flatten)
      }
      case _ => Failure(Seq("Not an array"))
    }
  }
}
