package json

import org.scalatest._

class JsonSpec extends FlatSpec with Matchers {

  val js = JObject(Map(
    "name" -> JString("Sam"),
    "age" -> JNumber(30),
    "students" -> JArray(Seq(
      JObject(Map(
        "name" -> JString("Vincent"),
        "age" -> JNumber(35),
        "students" -> JArray(Seq())
      )),
      JObject(Map(
        "name" -> JString("Fabien"),
        "age" -> JNumber(30),
        "students" -> JArray(Seq())
      ))
    ))
  ))

  val sam = Zengular("Sam", 30, List(Zengular("Vincent", 35), Zengular("Fabien", 30)))

  "Json Decoder" should "decode a String" in {
    JString("Hello").validate[String] shouldEqual Success("Hello")
  }

  "Json decoder" should "decode a class" in {
    js.validate[Zengular] shouldEqual Success(sam)
  }

  "Json Encoder" should "encode a Zengular" in {
    JValue.toJson(sam) shouldEqual js
  }
}
