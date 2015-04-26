package unicorn

import simulacrum._

@typeclass trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  // List(("a",1)("b",2)) => (List("a","b"),List(1,2)) aka unZip
  def distribute[A,B](fab: F[(A,B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))
}

object Functor {

  implicit def ListFunctor: Functor[List] = new Functor[List] {
    def map[A, B](la: List[A])(f: A => B): List[B] = la map f
  }

  implicit def OptionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](oa: Option[A])(f: A => B): Option[B] = oa map f
  }


}
