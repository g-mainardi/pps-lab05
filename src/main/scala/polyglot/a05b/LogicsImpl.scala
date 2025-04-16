package polyglot.a05b

import polyglot.a05b.Logics
import util.SetADT
import SetADT.*
import util.Sequences.*
import Sequence.*

import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:
  opaque type Position = (Int, Int)
  private val init: Position = randomPosition()
  private var offset: Int = 1
  private var s: Set[Position] = SetADT.fromSequence(Cons(init, Nil()))

  private def randomPosition(seed: Int = 42): Position =
    val rand = Random(seed)
    def rndCord = 1 + rand.nextInt(size - 2)
    (rndCord, rndCord)

  override def tick(): Unit =
    for
      dir_x <- -1 to 1
      dir_y <- -1 to 1
      if dir_x != 0 || dir_y != 0
    do s = s.put(init._1 + offset * dir_x, init._2 + offset * dir_y)
    offset += 1

  private object OverBorder:
    def unapply(pos: Position): Boolean =
      pos._1 < 0 || pos._1 > size || pos._2 < 0 || pos._2 > size

  override def isOver: Boolean = s anyMatch {
    case OverBorder() => true
    case _ => false
  }

  override def hasElement(x: Int, y: Int): Boolean = s.contains((x,y))

@main def testLogics(): Unit =
  val size = 5
  val log = new LogicsImpl(size)
  1 to size foreach(_ => log.tick())
  println("Is over? " + log.isOver)

