package tasks.adts
import u03.Sequences.*
import u03.Optionals.*

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestion: 
 *  - reuse Sequences and Optionals as imported above
 *  - Course is a simple case classes with just the name
 *  - Teacher is a case class with name and sequence of courses
 *  - School is a case class with (sequences of) teachers and courses
 *  - add/set methods below create the new school 
 */

object SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course
    extension (school: School)
      def addTeacher(name: String): School
      def addCourse(name: String): School
      def teacherByName(name: String): Optional[Teacher]
      def courseByName(name: String): Optional[Course]
      def nameOfTeacher(teacher: Teacher): String
      def nameOfCourse(teacher: Course): String
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]

  object  BasicSchoolModule extends SchoolModule:
    private case class CourseImpl(name: String)
    private case class TeacherImpl(name: String, courses: Sequence[Course])
    private case class SchoolImpl(teachers: Sequence[Teacher], courses: Sequence[Course])

    opaque type Course = CourseImpl
    opaque type Teacher = TeacherImpl
    opaque type School = SchoolImpl

    extension (school: School)
      def addTeacher(name: String): School = school match
        case School(teachers, courses) =>  SchoolImpl(Sequence.Cons(TeacherImpl(name, Sequence.Nil()), teachers), courses)
      
      def addCourse(name: String): School = school match
        case School(teachers, courses) =>  SchoolImpl(teachers, Sequence.Cons(CourseImpl(name), courses))
      def teacherByName(name: String): Optional[Teacher] = 
        def getTeacher(name: String, teachers:Sequence[TeacherImpl]): Optional[TeacherImpl] = teachers match
          case Sequence.Cons(TeacherImpl(t,y), _) if t == name => Optional.Just(TeacherImpl(t,y))
          case Sequence.Cons(TeacherImpl(t,y), te) => getTeacher(name, te)
          case Sequence.Nil() => Optional.Empty()
        getTeacher(name, school.teachers)
        
      
      def courseByName(name: String): Optional[Course] = ???
      def nameOfTeacher(teacher: Teacher): String = ???
      def nameOfCourse(teacher: Teacher): String = ???
      def setTeacherToCourse(teacher: Teacher, course: Course): School = ???
      def coursesOfATeacher(teacher: Teacher): Sequence[Course] = ???


