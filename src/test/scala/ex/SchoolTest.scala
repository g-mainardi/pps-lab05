package ex

import ex.SchoolModel.*
import util.Sequences.*
import Sequence.*

import org.junit.*
import org.junit.Assert.*


class SchoolTest:

  val schoolADT: SchoolModule = BasicSchoolModule
  import schoolADT.*

  @Test def testTeacher(): Unit =
    assertEquals("John", teacher("John"))

  @Test def testCourse(): Unit =
    assertEquals("Math", course("Math"))

  @Test def testEmptySchool(): Unit =
    assertEquals(Nil(), emptySchool)

  @Test def testGetCourses(): Unit =
    val noCourses = emptySchool.courses
    val oneCourse = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math")).courses
    val twoCourses = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))
      .setTeacherToCourse(teacher("John"), course("Italian")).courses
    val duplicatedCourses = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))
      .setTeacherToCourse(teacher("Peter"), course("Math")).courses
    assertEquals(Nil(), noCourses)
    assertEquals(Cons("Math", Nil()), oneCourse)
    assertEquals(Cons("Math", Nil()), duplicatedCourses)

  @Test def testGetTeachers(): Unit =
    val noTeachers = emptySchool.teachers
    val oneTeacher = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math")).teachers
    val duplicatedTeacher = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))
      .setTeacherToCourse(teacher("John"), course("Italian")).teachers
    assertEquals(Nil(), noTeachers)
    assertEquals(Cons("John", Nil()), oneTeacher)
    assertEquals(Cons("John", Nil()), duplicatedTeacher)

  @Test def testSetTeacherToCourse(): Unit =
    val mySchool = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))
    assertEquals(Cons("Math", Nil()), mySchool.courses)
    assertEquals(Cons("John", Nil()), mySchool.teachers)

  @Test def testCoursesOfATeacher(): Unit =
    val coursesSchool1 = emptySchool
      .coursesOfATeacher(teacher("John"))
    val coursesSchool2 = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))
      .coursesOfATeacher(teacher("John"))
    val coursesSchool3 = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))
      .setTeacherToCourse(teacher("John"), course("Italian"))
      .coursesOfATeacher(teacher("John"))
    assertEquals(Nil(), coursesSchool1)
    assertEquals(Cons("Math", Nil()), coursesSchool2)
    assertEquals(Cons("Math", Cons("Italian", Nil())), coursesSchool3)

  @Test def testHasTeacher(): Unit =
    val school1HasJohn = emptySchool
      .hasTeacher("John")
    val school2HasJohn = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))
      .hasTeacher("John")
    assertFalse(school1HasJohn)
    assertTrue(school2HasJohn)

  @Test def testHasCourse(): Unit =
    val school1HasMath = emptySchool
      .hasCourse("Math")
    val school2HasMath = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))
      .hasCourse("Math")
    assertFalse(school1HasMath)
    assertTrue(school2HasMath)
