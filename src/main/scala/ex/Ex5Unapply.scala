package ex

import util.Sequences.*
import util.Optionals.*
import Optional.*
import util.Sequences.Sequence.Cons

import scala.annotation.tailrec

def optionalToOption[A](our: Optional[A]): Option[A] = our match
  case Just(a) => Some(a)
  case _ => None

object sameCategory:
  def unapply(courses: Sequence[Course]): Option[String] =
    @tailrec
    def _allSameCategory(s: Sequence[Course])(cat: Optional[String]): Optional[String] = s match
      case Cons(h, t) => _allSameCategory(t)(cat.filter(_ == h.category))
      case _ => cat
    optionalToOption(_allSameCategory(courses)(courses.head.map(_.category)))

@main def testExtractor(): Unit =
  val scalaCourse = Course("SCALA01", "Functional Programming in Scala", "Prof. Odersky", "Programming")
  val pythonCourse = Course("PYTHON01", "Introduction to Python", "Prof. van Rossum", "Programming")
  val designCourse = Course("DESIGN01", "UI/UX Design Fundamentals", "Prof. Norman", "Design")

  def printCategories(courses: Sequence[Course]): Unit = courses match
      case sameCategory(cat) => println(s" $courses have same category $cat")
      case _ => println(s" $courses have different categories")

  printCategories(Sequence(scalaCourse, pythonCourse, designCourse))
  printCategories(Sequence(scalaCourse, pythonCourse, designCourse, scalaCourse, pythonCourse))
  printCategories(Sequence(scalaCourse, pythonCourse))
  printCategories(Sequence(designCourse))
