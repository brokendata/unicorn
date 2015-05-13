import scalaz._
import Scalaz._

sealed trait Result[+A]
final case class Success[A](value: A) extends Result[A]
final case class Warning[A](value: A, message: String) extends Result[A]
final case class Failure(message: String) extends Result[Nothing]

implicit val resultFunctor = new Functor[Result] {
  def map[A, B](result: Result[A])(func: A => B): Result[B] =
    result match {
      case Success(value) => Success(func(value))
      case Warning(value, message) => Warning(func(value), message)
      case Failure(message) => Failure(message)
    }
}

implicit def resultShow[A] = Show.shows[Result[A]]{r =>
"result"}
Show[Result[Int]].shows(Success(1))