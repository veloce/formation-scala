package typeclass

trait DoesCry[-T] {
  def cry(t: T): String
}

sealed trait Animal

case object Cat extends Animal
case object Dog extends Animal

object Cryable {
  implicit val animalCry = new DoesCry[Animal] {
    def cry(a: Animal) = a match {
      case Cat => "meow"
      case Dog => "Whoof whoof"
      case _ => "not an animal"
    }
  }
}

object Call {
  def call[A : DoesCry](x: A) = {
    val d = implicitly[DoesCry[A]]
    d.cry(x)
  }
}
