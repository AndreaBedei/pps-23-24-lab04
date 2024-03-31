// package tasks

// import org.junit.Assert._
// import org.junit.Test
// import u03.Optionals._
// import u03.Sequences.Sequence


// class SchoolTest:

//   @Test def testAddTeacher() = 
//     val school = SchoolImpl(Sequence.Nil(), Sequence.Nil())
//     val updatedSchool = school.addTeacher("John")
//     assertEquals(Sequence.Cons(TeacherImpl("John", Sequence.Nil()), Sequence.Nil()), updatedSchool.teachers)
  

//   @Test def testAddCourse() = 
//     val school = SchoolImpl(Sequence.Nil(), Sequence.Nil())
//     val updatedSchool = school.addCourse("Math")
//     assertEquals(Sequence.Cons(CourseImpl("Math"), Sequence.Nil()), updatedSchool.courses)

//   @Test def testTeacherByName() = 
//     val teacher = TeacherImpl("John", Sequence.Nil())
//     val school = SchoolImpl(Sequence.Cons(teacher, Sequence.Nil()), Sequence.Nil())
//     assertEquals(Optional.Just(teacher), school.teacherByName("John"))
//     assertEquals(Optional.Empty(), school.teacherByName("Jane"))

//   @Test def testCourseByName() = 
//     val course = CourseImpl("Math")
//     val school = SchoolImpl(Sequence.Nil(), Sequence.Cons(course, Sequence.Nil()))
//     assertEquals(Optional.Just(course), school.courseByName("Math"))
//     assertEquals(Optional.Empty(), school.courseByName("Physics"))

//   @Test def testSetTeacherToCourse() =
//     val teacher = TeacherImpl("John", Sequence.Nil())
//     val school = SchoolImpl(Sequence.Cons(teacher, Sequence.Nil()), Sequence.Nil())
//     val updatedSchool = school.setTeacherToCourse(teacher, CourseImpl("Math"))
//     assertEquals(Optional.Just(Sequence.Cons(CourseImpl("Math"), Sequence.Nil())), updatedSchool.teachers.headOption.map(_.courses))

//   @Test def testCoursesOfATeacher() =
//     val teacher = TeacherImpl("John", Sequence.Cons(CourseImpl("Math"), Sequence.Nil()))
//     assertEquals(Sequence.Cons(CourseImpl("Math"), Sequence.Nil()), coursesOfATeacher(teacher))
  
