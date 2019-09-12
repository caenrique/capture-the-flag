package com.uhu.cesar.ctf.utilities

object OptionUtils {

  private def map2[E, A, EE >: E, B, C](a: Option[A])(b: Option[B])(f: (A, B) => C) = for {a1 <- a; b1 <- b} yield f(a1, b1)

  private def traverse[AA, B](os: List[AA])(f: AA => Option[B]): Option[List[B]] = os match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h))(traverse(t)(f))(_ :: _)
  }

  implicit class OptionListCanSequence[A](ol: List[Option[A]]) {
    def sequence: Option[List[A]] = traverse(ol)(x => x)
  }

}
