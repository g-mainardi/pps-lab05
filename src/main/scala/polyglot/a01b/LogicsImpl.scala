package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import polyglot.a01b.Logics
import util.SetADT
import SetADT.*

import scala.jdk.javaapi.OptionConverters
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
trait LogicsTrait extends Logics:
  def hit(x: Int, y: Int): java.util.Optional[Integer]
  def won: Boolean

class LogicsImpl(private val size: Int, private val mines: Int) extends LogicsTrait:
  opaque type Position = (Int, Int)
  private var cleanHit: Int = 0
  private val rand = Random(42)
  private var minesPositions: Set[Position] = SetADT.empty
  while (minesPositions.size < mines)
    minesPositions = minesPositions.put((rand.nextInt(size), rand.nextInt(size)))
  println("Mines positions: " + minesPositions)

  private def clear(): Unit = cleanHit += 1
  
  private object isMine:
    def unapply(pos: Position): Boolean = minesPositions.contains(pos)

  override def hit(x: Int, y: Int): java.util.Optional[Integer] =
    OptionToOptional( (x, y) match
      case isMine() => ScalaOptional.Empty()
      case _ => clear(); ScalaOptional.Just(
        (
          for
            dx <- -1 to 1
            dy <- -1 to 1
            if dx != 0 || dy != 0
            if minesPositions.contains(x + dx, y + dy)
          yield 1
        ).size)
    )

  override def won: Boolean = cleanHit >= (size * size - mines)
