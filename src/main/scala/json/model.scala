package json

case class Zengular(name: String, age: Int, students: Seq[Zengular] = Seq())

object Zengular {
  import BuilderOps._

  // implicit val decoder: Decoder[Zengular] = {
  //   import Decoder._
  //   (
  //     fieldDecoder[String]("name") and
  //     fieldDecoder[Int]("age") and
  //     fieldDecoder[Seq[Zengular]]("students")
  //   ) map {
  //     case ((name, age), trainees) => Zengular(name, age, trainees)
  //   }
  // }

  // implicit val encoder: Encoder[Zengular] = {
  //   import Encoder._
  //   (
  //     fieldEncoder[String]("name") and
  //     fieldEncoder[Int]("age") and
  //     fieldEncoder[Seq[Zengular]]("students")
  //   ) contramap {
  //     case Zengular((name, age), trainees) => (name, age, trainees)
  //   }
  // }

  implicit val formatter: Formatter[Zengular] = {
    import Formatter._
    (
      fieldFormatter[String]("name") and
      fieldFormatter[Int]("age") and
      fieldFormatter[Seq[Zengular]]("students")
    ) inmap (
      {
        case ((name, age), trainees) => Zengular(name, age, trainees)
      },
      {
        case Zengular(name, age, trainees) => ((name, age), trainees)
      }
    )
  }


}
