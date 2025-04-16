package util

import Sequences.*
import Sequence.*

import scala.annotation.tailrec

object SetADT:
  opaque type Set[A] = Sequence[A]

  def fromSequence[A](s: Sequence[A]): Set[A] = s match
    case Cons(h, t) => Cons(h, fromSequence(t.remove(h)))
    case Nil()      => Nil()

  def union[A](s1: Set[A], s2: Set[A]): Set[A] = s2 match
    case Cons(h, t) => Cons(h, union(s1.remove(h), t))
    case Nil()      => s1

  def intersection[A](s1: Set[A], s2: Set[A]): Set[A] = s1 match
    case Cons(h, t) if s2.contains(h) => Cons(h, intersection(t, s2.remove(h)))
    case Cons(_, t) => intersection(t, s2)
    case Nil()      => Nil()

  extension [A](s: Set[A])
    @tailrec
    def contains(a: A): Boolean = s match
      case Cons(h, t) if h == a => true
      case Cons(_, t)           => t.contains(a)
      case Nil()                => false
    def remove(a: A): Set[A] = s.filter(_ != a)  
    def toSequence: Sequence[A] = s
    def put(a: A): Set[A] = if contains(a) then s else Cons(a, s)
    @tailrec
    def allMatch(f: A => Boolean): Boolean = s match
      case Cons(h, Nil()) => f(h)
      case Cons(h, t) => f(h) && t.allMatch(f)
      case _ => false
    @tailrec
    def anyMatch(f: A => Boolean): Boolean = s match
      case Cons(h, t) => f(h) || t.anyMatch(f)
      case _ => false

@main def trySetADT(): Unit =
  import SetADT.*
  val s1: Set[Int] = fromSequence(Cons(10, Cons(20, Cons(10, Cons(30, Nil())))))
  val s2: Set[Int] = fromSequence(Cons(10, Cons(11, Nil())))
  val s3: Set[Int] = s2.put(22)
  // val s3: Set[Int] = Cons(10, Cons(11, Cons(22, Nil()))) // because Set is defined opaque
  val emptySet: Set[Int] = fromSequence(empty)
  println(s1 allMatch(_ > 0))              // true
  println(s1 allMatch(_ > 10))             // false
  println(emptySet allMatch(_ => true))    // false

  println(s1 anyMatch(_ == 10))            // true
  println(s1 anyMatch(_ > 40)  )           // false
  println(emptySet anyMatch(_ => false))   // false

  println(s1.toSequence)                   // (10, 20, 30)
  println(s2.toSequence)                   // (10, 11)
  println(s3.toSequence)                   // (10, 11, 22)
  println(union(s1, s2).toSequence)        // (10, 20, 30, 11)
  println(intersection(s1, s2).toSequence) // (10)
