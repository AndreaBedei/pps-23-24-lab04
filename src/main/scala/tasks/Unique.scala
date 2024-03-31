package tasks

object Unique :
  package tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

    object Ex1ComplexNumbers:
        // Task 1
        trait ComplexADT:
            type Complex
            def complex(re: Double, im: Double): Complex
            extension (complex: Complex)
            def re(): Double
            def im(): Double
            def sum(other: Complex): Complex
            def subtract(other: Complex): Complex
            def asString(): String

        object BasicComplexADT extends ComplexADT:
            private case class ComplexNumber(real: Double, img: Double)

            opaque type Complex = ComplexNumber 

            def complex(re: Double, im: Double): Complex = ComplexNumber(re, im)
            
            extension (complex: Complex)
            def re(): Double = complex match
                case Complex(r, _) => r
            
            def im(): Double = complex match
                case Complex(_, i) => i
            
            def sum(other: Complex): Complex = complex match
                case Complex(r, i) => ComplexNumber(r + other.real, i + other.img)
            
            def subtract(other: Complex): Complex = complex match
                case Complex(r, i) => ComplexNumber(r - other.real, i - other.img)

            def asString(): String = complex match
                case Complex(0, 0) => "0.0"
                case Complex(0, i) => i +"i"
                case Complex(r, 0) => r + ""
                case Complex(r, i) if i < 0 => r + " - " +  -i +"i"
                case Complex(r, i) => r + " + " + i +"i"  

    // Task 2  
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

    object BasicSchoolModule extends SchoolModule:

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
            
        def courseByName(name: String): Optional[Course] =
            def getCourse(name: String, courses:Sequence[CourseImpl]): Optional[CourseImpl] = courses match
            case Sequence.Cons(CourseImpl(n), pr) if n == name => Optional.Just(CourseImpl(n))
            case Sequence.Nil() => Optional.Empty()
            getCourse(name, school.courses)
        
        def nameOfTeacher(teacher: Teacher): String = teacher.name
        def nameOfCourse(course: Course): String = course.name
        def setTeacherToCourse(teacher: Teacher, course: Course): School = 
            val mapTeachers: Teacher => Teacher = (t) => (t, teacher) match
                case (TeacherImpl(n1, c), TeacherImpl(n2, _)) if n1 == n2 => TeacherImpl(n1, Sequence.Cons(course, c))
                case _ => t
            school.match
            case SchoolImpl(teachers, courses) if teachers != Sequence.Nil() && courses != Sequence.Nil() =>
            SchoolImpl(Sequence.map(teachers: Sequence[TeacherImpl])(mapTeachers), courses)
            case _ => school

        def coursesOfATeacher(teacher: Teacher): Sequence[Course] = teacher.courses

    // Task3

    object Ex3Stacks:
        trait StackADT:
            type Stack[A]
            def empty[A]: Stack[A] // factory
            extension [A](stack: Stack[A])
            def push(a: A): Stack[A]
            def pop(a: A): Optional[(A, Stack[A])]
            def asSequence(): Sequence[A]
        
        object StackImpl extends StackADT:
            type Stack[A] = Sequence[A]
            def empty[A]: Stack[A] = Sequence.Nil()

            extension [A](stack: Stack[A])
            def push(a: A): Stack[A] = Sequence.Cons(a, stack)
            
            def pop(a: A): Optional[(A, Stack[A])] = stack match
                case Sequence.Cons(head, tail) => Optional.Just(head, tail)      
                case _ => Optional.Empty()
            
            def asSequence(): Sequence[A] = stack 

    // Task 4
    object Ex4Summables:
        def sumAllInt(seq: Sequence[Int]): Int = seq match
            case Cons(h, t) => h + sumAllInt(t)
            case _ => 0

        trait Summable[A]:
            def sum(a1: A, a2: A): A
            def zero: A

        def sumAll[A: Summable](seq: Sequence[A]): A = 
            val summable = summon[Summable[A]]
            seq match
            case Cons(h, t) => summable.sum(h, sumAll(t))
            case _ => summable.zero

        given Summable[Int] with
            def sum(a1: Int, a2: Int): Int = a1 + a2
            def zero: Int = 0

        given Summable[Double] with
            def sum(a1: Double, a2: Double): Double = a1 + a2
            def zero: Double = 0.0

        given Summable[String] with
            def sum(a1: String, a2: String): String = a1 + a2
            def zero: String = ""

    // Task 5

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

    // Task 6
    object Ex6TryModel:
        private enum TryImpl[A]:
            case Success(value: A)
            case Failure(exception: Throwable)

        opaque type Try[A] = TryImpl[A]

        def success[A](value: A): Try[A] = TryImpl.Success(value)
        def failure[A](exception: Throwable): Try[A] = TryImpl.Failure(exception)
        def exec[A](expression: => A): Try[A] = try success(expression) catch failure(_)

        extension [A](m: Try[A]) 
            def getOrElse[B >: A](other: B): B = m match
            case TryImpl.Success(value) => value
            case TryImpl.Failure(_) => other

        given Monad[Try] with
            override def unit[A](value: A): Try[A] = success(value)
            extension [A](m: Try[A]) 
            override def flatMap[B](f: A => Try[B]): Try[B] =  m match
                case TryImpl.Success(value) => f(value)
                case TryImpl.Failure(exception) => failure(exception)