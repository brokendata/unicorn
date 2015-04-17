package unicorn

import scala.language.higherKinds
import unicorn.ADT._

trait Monad[M[_]] extends Functor[M] {
  //Bind
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  //Point
  def unit[A](a: A): M[A]

  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(f andThen unit)

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  // Monadic Combinators
  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List.empty[A]))((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List.empty[B]))((a, mlb) => map2(f(a), mlb)(_ :: _))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = {
    def loop(n: Int, acc: M[List[A]]): M[List[A]] =
      if (n <= 0) acc
      else loop(n - 1, map2(ma, acc)(_ :: _))
    loop(n,unit(List.empty[A]))
  }

  def factor[A,B](ma: M[A], mb: M[B]): M[(A,B)] = map2(ma,mb)((_,_))

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

  implicit def IdMonad: Monad[Id] = new Monad[Id]{
    def flatMap[A,B](ia: Id[A])(f: A => Id[B]): Id[B] = ia match {
      case Id(a) => f(a)
    }

    def unit[A](a: A): Id[A] = Id(a)
  }


}
