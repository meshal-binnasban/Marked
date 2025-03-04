error id: ammonite/$file/play_semi_bool.Rexp.SEQ#[S]
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/play_semi_bool.sc
empty definition using pc, found symbol in pc: 
empty definition using semanticdb
|empty definition using fallback
non-local guesses:
	 -Rexp.S#
	 -REw.S#
	 -S#
	 -scala/Predef.S#

Document text:

```scala


enum Rexp[C, S] {
  case ZERO()
  case ONE() 
  case CHAR(f: C => S)
  case ALT(r1: Rexp[C, S], r2: Rexp[C, S])
  case SEQ(r1: Rexp[C, S], r2: Rexp[C, S])
  case STAR(r: Rexp[C, S])
}

import Rexp._

// regular expressions with weights
enum REw[C, S] {
  case ZEROw()
  case ONEw() 
  case CHARw(b: S, f: C => S) 
  case ALTw(a: REw[C, S], b: REw[C, S]) 
  case SEQw(a: REw[C, S], b: REw[C, S]) 
  case STARw(a: REw[C, S]) 
}

import REw._

def nullable[C, S](r: REw[C, S])(using semi: Semiring[S]) : S = r match {
  case ZEROw() => semi.zero
  case ONEw() => semi.one
  case CHARw(_, f) => semi.zero
  case ALTw(a, b) => semi.plus(nullable(a), nullable(b))
  case SEQw(a, b) => semi.times(nullable(a), nullable(b))
  case STARw(a) => semi.one
}
 
def fin[C, S](r: REw[C, S])(using semi: Semiring[S]) : S = r match {
  case ZEROw() => semi.zero
  case ONEw() => semi.zero
  case CHARw(b, f) => b
  case ALTw(a, b) => semi.plus(fin(a), fin(b))
  case SEQw(a, b) => semi.plus(semi.times(fin(a), nullable(b)), fin(b))
  case STARw(a) => fin(a)
}

def shift[C, S](m: S, re: REw[C, S], c: C)(using semi: Semiring[S]): REw[C, S] = {
  re match {
    case ZEROw() => ZEROw()
    case ONEw() => ONEw()
    case CHARw(b, f) => CHARw(semi.plus(b, f(c)), f) 
    case ALTw(r1, r2) => ALTw(shift(m, r1, c), shift(m, r2, c))
    case SEQw(r1, r2) => 
      SEQw(shift(m, r1, c),
           shift(semi.plus(semi.times(m, nullable(r1)), fin(r1)), r2, c))
    case STARw(r) => STARw(shift(semi.plus(m, fin(r)), r, c))
  }
}

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

def matcher[C, S](r: REw[C, S], s: List[C])(using semi: Semiring[S]): S = {
    s match {
      case Nil => nullable(r)
      case c :: cs => val  x = cs.foldLeft(shift(semi.one, r, c))
        ((r, c) => shift(semi.zero, r, c))
        fin(x)
    }
  }

def intern[C, S](r: Rexp[C, S])(using semi: Semiring[S]): REw[C, S] = r match {
  case ZERO()      => ZEROw()
  case ONE()       => ONEw()
  case CHAR(f)   => CHARw(semi.zero, f)
  case ALT(r1, r2)  => ALTw(intern(r1), intern(r2))
  case SEQ(r1, r2)  => SEQw(intern(r1), intern(r2))
  case STAR(r1)     => STARw(intern(r1))
}

@main
def test1() = {
  val a = CHAR(_ == 'a')
  val b = CHAR(_ == 'b')
  val reg = SEQ(a, b)
  val str = "ab".toList
  
  println("testing new implementation")
  println(matcher(intern(reg), str))
}

// input being a list of (Characters, Ints)
@main
def test2() = {
  val a = CHAR[(Char, Int), Boolean](_._1 == 'a')
  val b = CHAR[(Char, Int), Boolean](_._1 == 'b')
  val reg = SEQ(a, b)
  val str = "ab".toList.zipWithIndex

  println("testing new implementation")
  println(matcher(intern(reg), str))
}

```

#### Short summary: 

empty definition using pc, found symbol in pc: 