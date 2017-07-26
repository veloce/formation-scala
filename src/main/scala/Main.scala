import expr._

import typeclass._
import json._


trait Simple {
  def value = 7
}

trait Doubling extends Simple {
  override def value = super.value * 2
}

trait Tripling extends Simple {
  override def value = super.value * 3
}

object Main extends App {

  val t = new Simple with Doubling with Tripling

  println(t.value)

  import Cryable._

  println(Call.call(Cat))

  val js = JObject(Map(
    "name" -> JString("Sam"),
    "students" -> JArray(Seq(
      JObject(Map(
        "name" -> JString("Vincent")
      )),
      JObject(Map(
        "name" -> JString("Fabien")
      ))
    ))
  ))

  println(js.show)
}
