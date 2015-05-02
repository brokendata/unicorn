package unicorn

trait IO[+A]{ self =>
  def run: A
  def map[B](f: A =>B): IO[B] =
    new IO[B]{def run = f(self.run)}
  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B]{def run = f(self.run).run}
}

object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A] { def run = a}
  def flatMap[A,B](ma: IO[A])(f: A => IO[B]) = ma flatMap f
  def apply[A](a: => A): IO[A] = unit(a)

 def ReadLine: IO[String] = IO{ readLine}
 def PrintLine(msg: String): IO[Unit] = IO{ println(msg) }

  /*
  We can now create an instance of IO[A] such as,
  IO[Unit], the type for printing / side effecting
  IO[String], for reading a val from the keyboard

  Our IO monad can now create RT IO in a monadic/ composable form
  For Example:

  val IntRead = ReadLine(_.toInt)

  or, what about a function that reads a line, performs and action
  and then prints the line (IO Monads Compose):

 */
  def add2: IO[Unit] = for {
    _ <- PrintLine("Enter a number that be added to 2")
    i <- ReadLine.map(_.toInt)
    _ <- PrintLine(s"The number is ${i+2}")
    } yield ()

}
