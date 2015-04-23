package unicorn.syntax
import unicorn.Equal
object EqualSyntax {
  /*
  Add implicit method === to any available instance of Equal typeclass
  Allows typesafe equality: 3 === "3" will not compile
   */

  implicit class EqualS[A: Equal](a: A){
    def ===(b: A): Boolean = implicitly[Equal[A]].equals(a,b)
  }


}
