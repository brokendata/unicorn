package unicorn.syntax
import unicorn.Monad
import unicorn.ADT.Id

object MonadSyntax {
  implicit class MSyntax[M[_]: Monad, A](ma: M[A]){
    def flatMap[B](f: A => M[B]) = implicitly[Monad[M]].flatMap(ma)(f)
    def map[B](f: A =>B) = implicitly[Monad[M]].map(ma)(f)
  }

  //add method to all datatypes that allows any data to be wrapped in an Id Monad
    // 1.id == Id(1)
  implicit class MId[A](a: A){
    def id: Id[A] = Monad[Id].unit(a)
  }
}
