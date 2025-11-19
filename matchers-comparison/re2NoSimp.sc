// A Version of the simple matcher with an explicit 
// n-times regular expression
//
// this keeps the size of the regular expression in the
// EVIL1 testcase small
//
// call the test cases with X = {1,2}
//
//   amm re2.sc testX
//
// or 
//
//   amm re2.sc all
import scala.language.implicitConversions

// regular expressions (as enum in Scala 3)
enum Rexp {
  case ZERO                     // matches nothing
  case ONE                      // matches an empty string
  case CHAR(c: Char)            // matches a character c
  case ALT(r1: Rexp, r2: Rexp)  // alternative
  case SEQ(r1: Rexp, r2: Rexp)  // sequence
  case STAR(r: Rexp)            // star
  case NTIMES(r: Rexp, n: Int)  // explicit n-times
}
import Rexp._


def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, n) => if (n == 0) true else nullable(r)
}

def der(c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => 
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r1) => SEQ(der(c, r1), STAR(r1))
  case NTIMES(r, n) => 
    if (n == 0) ZERO else SEQ(der(c, r), NTIMES(r, n - 1))
}

def ders(s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, der(c, r))
}

def matcher(r: Rexp, s: String) : Boolean = 
  nullable(ders(s.toList, r))


// Test Cases

// the optional regular expression: one or zero times
// this regular expression is still defined in terms of ALT
def OPT(r: Rexp) = ALT(r, ONE)


// evil regular expressions
def EVIL1(n: Int) = SEQ(NTIMES(OPT(CHAR('a')), n), NTIMES(CHAR('a'), n))
val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}


def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

// strings are coerced into Rexps
given Conversion[String, Rexp] = (s => charlist2rexp(s.toList))

//val HELLO : Rexp = "hello"

extension (r: Rexp) {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}


@main
def test1() = {
  val r = %( %("a") | %("aa") | %("aaa") | %("aaaa") | %("aaaaa") ) 
  println(s"Original Size=${size(r)}")
  for (i <- 1 to 100 by 1) {
    val s = "a" * i  //+ "b"         
    val der=ders(s.toList,r)
   // println(pp(der))
    println(s"i=$i , size= ${size(der)}")
    //println(s" ${bders_simp(internalise(r),s.toList)}")
    
  }

}

// the size of a regular expressions - for testing purposes 
def size(r: Rexp) : Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_) => 1
  case ALT(r1, r2) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2) => 1 + size(r1) + size(r2)
  case STAR(r) => 1 + size(r)
  case NTIMES(r, _) => 1 + size(r)
}

