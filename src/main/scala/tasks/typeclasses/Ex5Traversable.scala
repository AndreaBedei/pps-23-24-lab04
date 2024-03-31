package u04lab
import u03.Sequences.* 
import Sequence.*
import u03.Optionals.Optional

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  def log[A](a: A): Unit = println("The next element is: "+a)

  def logAll[A](seq: Sequence[A]): Unit = seq match
    case Cons(h, t) => log(h); logAll(t)
    case _ => ()

  trait Traversable[T[_]]:
    extension [A](el: T[A])
      def traverse(consumer: A => Unit): Unit

  given Traversable[Optional] with
    extension [A](el: Optional[A])
      def traverse(consumer: A => Unit): Unit = el match
        case Optional.Just(a) => consumer(a)
        case _ => ()

  given Traversable[Sequence] with
    extension [A](el: Sequence[A])
      def traverse(consumer: A => Unit): Unit = el match
        case Cons(h, t) =>
          consumer(h)
          t.traverse(consumer)
        case _ => ()

  
  @main def traversablePrint =
      val sequenza1: Sequence[String] = Cons("a", Cons("i", Cons("o", Cons("u", Nil()))))
      sequenza1.traverse(log)

      val optional1: Optional[String] = Optional.Just("stringa")
      optional1.traverse(log)

      val optional2: Optional[Int] = Optional.Just(1)
      optional2.traverse(log)

      val optional3: Optional[Int] = Optional.Empty[Int]()
      optional3.traverse(log)