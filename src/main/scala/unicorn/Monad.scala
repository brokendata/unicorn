package unicorn

import scala.language.higherKinds

trait Monad[M[_]] extends Functor[M] {
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def unit[A](a: A): M[A]

  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(f andThen unit)

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  // Monadic Combinators 
  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List.empty[A]))((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List.empty[B]))((a, mlb) => map2(f(a), mlb)(_ :: _))
}

object Monad {
  def apply[M[_] : Monad] = implicitly[Monad[M]]

  implicit def ListMonad: Monad[List] = new Monad[List] {
    def flatMap[A, B](la: List[A])(f: A => List[B]): List[B] = la flatMap f

    def unit[A](a: A) = List(a)
  }

  implicit def OptionMonad: Monad[Option] = new Monad[Option] {
    def flatMap[A, B](oa: Option[A])(f: A => Option[B]): Option[B] = oa flatMap f

    def unit[A](a: A) = Some(a)
  }
}
