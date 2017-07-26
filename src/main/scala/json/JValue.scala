package json

sealed trait JValue {
  def show: String = {
    def doShow(indent: String)(jvalue: JValue): String = jvalue match {
      case JNumber(n) => n.toString
      case JString(s) => '"' +: s.replaceAll("\"", "\\\"") :+ '"'
      case JBoolean(b) => b.toString
      case JNull => "null"
      case JArray(els) => els.map(doShow(indent)).mkString("[", ", ", "]")
      case JObject(fields) => {
        fields.map { case (k, v) =>
          s"""\"$k\": ${doShow(indent ++ " ")(v)}"""
        }.mkString("{\n  " ++ indent, ",\n  " ++ indent, "\n" ++ indent ++ "}")
      }
    }
    doShow("")(this)
  }

  def validate[T : Decoder]: Result[T] = {
    val v = implicitly[Decoder[T]]
    v.decode(this)
  }
}

case class JNumber(n: Double) extends JValue
case class JString(s: String) extends JValue
case class JBoolean(b: Boolean) extends JValue
case object JNull extends JValue
case class JArray(els: Seq[JValue]) extends JValue

case class JObject(fields: Map[String, JValue]) extends JValue {
  def merge(o: JObject) = JObject(this.fields ++ o.fields)
}

object JValue {
  def toJson[T : Encoder](v: T): JValue = {
    val e = implicitly[Encoder[T]]
    e.encode(v)
  }
}
