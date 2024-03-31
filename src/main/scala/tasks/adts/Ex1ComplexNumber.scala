package tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

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

      
