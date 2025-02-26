error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-Weightedv2.sc:23
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-Weightedv2.sc
empty definition using pc, found symbol in pc: 
semanticdb not found
|empty definition using fallback
non-local guesses:
	 -Rexp.Rexp.CHAR.
	 -Rexp.Rexp.CHAR#
	 -Rexp.Rexp.CHAR().
	 -Rexp.CHAR.
	 -Rexp.CHAR#
	 -Rexp.CHAR().
	 -scala/Predef.Rexp.CHAR.
	 -scala/Predef.Rexp.CHAR#
	 -scala/Predef.Rexp.CHAR().

Document text:

```scala
/*
enum Rexp {
  case ZERO
  case ONE 
  case CHAR(c: Char , marked:Boolean = false)
  case ALT(r1: Rexp, r2: Rexp )
  case SEQ(r1: Rexp, r2: Rexp)
  case STAR(r: Rexp)
  case NTIMES(r: Rexp, n: Int) // from re4.sc
}
import Rexp._

*/
enum Rexp[C, S] {
  case ZERO extends Rexp[Nothing, Nothing]
  case ONE extends Rexp[Nothing, Nothing]
  case CHAR(f: C => S) 
  case ALT(r1: Rexp[C, S], r2: Rexp[C, S])
  case SEQ(r1: Rexp[C, S], r2: Rexp[C, S])
  case STAR(r: Rexp[C, S])
  case NTIMES(r: Rexp[C, S], n: Int)
}
def CHARb[C,S](c: Char)(using semiring: Semiring[S]): Rexp[Char, S] =
  Rexp.CHAR( x => if (x == c ) semiring.one else semiring.zero)

import Rexp._

trait Semiring[S] {
  def zero: S   // Additive identity
  def one: S    // Multiplicative identity
  def plus(a: S, b: S): S  // ⊕ Addition
  def times(a: S, b: S): S // ⊗ Multiplication
}

given booleanSemiring: Semiring[Boolean] with {
  def zero: Boolean = false
  def one: Boolean = true
  def plus(a: Boolean, b: Boolean): Boolean = a || b
  def times(a: Boolean, b: Boolean): Boolean = a && b
}
// maybe change to old? since this is not related to counting for example?
def nullable[C, S](r: Rexp[C, S])(using semiring: Semiring[S]): S = r match {
  case ZERO => semiring.zero
  case ONE => semiring.one
  case CHAR(_) => semiring.zero
  case ALT(r1, r2) => semiring.plus(nullable(r1), nullable(r2))
  case SEQ(r1, r2) => semiring.times(nullable(r1), nullable(r2))
  case STAR(_) => semiring.one
  case NTIMES(r, i) => if (i == 0) semiring.one else nullable(r)
}
/*
def nullable[C,S](r: Rexp[C,S]) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, i) => if (i == 0) true else nullable(r) //?
}
*/
// from re1.sc
//def OPT(r: Rexp[C,S]) = ALT(r, ONE)
def fin[C, S](r: Rexp[C, S])(using semiring: Semiring[S]): S = r match {
  case ZERO => semiring.zero
  case ONE => semiring.zero
  case CHAR(f) => semiring.zero // ?????
  case ALT(r1, r2) => semiring.plus(fin(r1),fin(r2))
  case SEQ(r1, r2) => 
    semiring.plus(semiring.times(fin(r1), nullable(r2)), fin(r2))
  case STAR(r) => fin(r)
  case NTIMES(r, i) =>  fin(r)// ? if (i == 0) false else
}
/*
def fin(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => false
  case CHAR(c,marked) => marked
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
  case NTIMES(r, i) =>  fin(r)// ? if (i == 0) false else
}
*/

//code for shift with

//def shift(mark: Boolean,r: Rexp,c: Char ): Rexp = r match {
def shift[C,S](mark: S, r: Rexp[C,S], c: Char)(using semiring: Semiring[S]): Rexp[C,S] = r match {
    case ZERO => ZERO
    case ONE=> ONE
    case CHAR(f) => CHAR(ch, c => semiring.times(mark, f(c)))
    
    case ALT(r1, r2) => ALT(shift(mark,r1,c),shift(mark,r2,c))
    case SEQ(r1,r2) => 
      SEQ (shift(mark,r1,c), 
          shift( semiring.plus
                              (semiring.times(mark , nullable(r1))  , fin(r1)) ,  r2,c))
    case STAR(r) => STAR(shift(semiring.plus(mark , fin(r)), r,c))
   
   /*
    case NTIMES(r, n) => 
      if(n==0) ONE[C,S]
      else SEQ(shift(semiring.plus(mark , fin(r)), r,c), NTIMES(r,n-1)) 
      */
}





def matcher(r: Rexp , s: List[Char]) : Boolean = s match {
case Nil => nullable(r)
case c :: s => fin(s.foldLeft(shift(true, r, c)) { (acc, c) =>
  shift(false, acc, c) })
}

@main
def test1() = {
//val r = SEQ(SEQ(CHAR('a'), CHAR('b')) , STAR(CHAR('c')))
//matcher(r, "abc".toList)
//matcher(r, "abcccccccccccccccccc".toList)

val r1=SEQ(NTIMES(OPT(CHAR('a')), 10), NTIMES(CHAR('a'), 10))
println("testing new ntimes")
matcher(r1, "aaaaaaaaaaaaaa".toList)

//val r= NTIMES(CHAR('b'),2)
//matcher(r, "bbc".toList)

}

def size(r: Rexp) : Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_,_) => 1
  case ALT(r1, r2) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2) => 1 + size(r1) + size(r2)
  case STAR(r) => 1 + size(r)
  case NTIMES(r, _) => 
    val x=1 + size(r)
    println("in size function: "+x)
    x
    
}

def EVIL1(n: Int) = 
  SEQ(NTIMES(OPT(CHAR('a')), n), NTIMES(CHAR('a'), n))

val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

@main
def test2() = {
  for (i <- 0 to 8000 by 1000) {
    println(f"$i: ${time_needed(2, matcher(EVIL1(i), ("a" * i).toList))}%.5f")
  }
}

//@arg(doc = "Test (a*)* b")
@main
def test3() = {
  for (i <- 0 to 7000000 by 500000) {
    println(f"$i: ${time_needed(2, matcher(EVIL2, ("a" * i).toList))}%.5f")
  }
} 

//@arg(doc = "All tests.")
@main
def all() = { test2(); test3() } 






```

#### Short summary: 

empty definition using pc, found symbol in pc: 