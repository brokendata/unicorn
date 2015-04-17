package unicorn

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

}

object Functor {

  def apply[F[_] : Functor] = implicitly[Functor[F]]

  implicit def ListFunctor: Functor[List] = new Functor[List] {
    def map[A,B](la: List[A])(f: A =>B): List[B]  = la map f
  }

  implicit def OptionFunctor: Functor[Option] = new Functor[Option]{
    def map[A,B](oa: Option[A])(f: A =>B): Option[B] = oa map f
  }


}
