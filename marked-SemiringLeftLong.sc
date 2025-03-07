import scala.compiletime.ops.boolean
import scala.math.min
import scala.language.implicitConversions

enum Rexp[C, S] {
  case ZERO()
  case ONE() 
  case CHAR(f: C => S)
  case ALT(r1: Rexp[C, S], r2: Rexp[C, S])
  case SEQ(r1: Rexp[C, S], r2: Rexp[C, S])
  case STAR(r: Rexp[C, S])
  case NTIMES(r: Rexp[C, S], n:Int)
}

import Rexp._
def OPT[C,S](r: Rexp[C,S]) = ALT(r, ONE())


enum REG[C,S] {
  case BZERO()
  case BONE() 
  case BCHAR(b: S, f: C => S)
  case BALT(r1: REG[C,S], r2: REG[C,S])
  case BSEQ(r1: REG[C,S], r2: REG[C,S])
  case BSTAR(r: REG[C,S])
  case BNTIMES(r: REG[C,S] , n:Int)
  case BINIT(r: REG[C,S])
}
import REG._
//BNTIMES() repeat with limit n, check if n+1 then stop?


def nullable[C,S](r: REG[C,S])(using semiring: SemiringI[S]) : S = r match {
  case BZERO() => semiring.zero
  case BONE()=> semiring.one
  case BCHAR(_,f) =>  semiring.zero
  case BALT(r1, r2) => semiring.plus(nullable(r1),nullable(r2))
  case BSEQ(r1, r2) => semiring.times(nullable(r1),nullable(r2))
  case BSTAR(r) => semiring.one
  case BNTIMES(r, n) => if (n == 0) semiring.one else nullable(r)
  case BINIT(r) => nullable(r)
}

def fin[C,S](r: REG[C,S])(using semiring: SemiringI[S]) : S = r match {
  case BZERO() => semiring.zero
  case BONE() => semiring.zero
  case BCHAR(b,_) => b
  case BALT(r1, r2) => semiring.plus( fin(r1) , fin(r2) )
  case BSEQ(r1, r2) => semiring.plus(semiring.times(fin(r1),nullable(r2)),fin(r2))
  case BSTAR(r) => fin(r)
  case BNTIMES(r, n) => if(n==0) semiring.zero else fin(r) // ? or just fin(r)
}

def shift[C,S](mark: S, re: REG[C,S], c: C)(using semiring: SemiringI[S]): REG[C,S] = {
    re match {
      case BZERO() => BZERO() // or just re 
      case BONE() => BONE() // or just re 
      case BCHAR(b,f) => BCHAR(semiring.times(mark, f(c)), f) 
      case BALT(r1, r2) => BALT(shift(mark, r1, c), shift(mark, r2, c))
      case BSEQ(r1, r2) => 
        BSEQ(shift(mark, r1, c),
             shift(semiring.plus(semiring.times(mark, nullable(r1)), fin(r1)), r2, c))
      case BSTAR(r) => BSTAR(shift(semiring.plus(mark, fin(r)), r, c))
      case BNTIMES(r, n) => if(n==0) BONE()  
                        else BSEQ(shift(semiring.plus(mark,fin(r)), r,c), BNTIMES(r,n-1)) 
      case BINIT(r) => shift(semiring.one, r, c)
    }
  }

trait Semiring[S] {
  def zero: S   // Additive identity
  def one: S    // Multiplicative identity
  def plus(a: S, b: S): S  // ⊕ Addition
  def times(a: S, b: S): S // ⊗ Multiplication
}

trait SemiringI[S] extends Semiring[S] {
    def index(i: Int): S

  }

sealed trait RangeT
case object NoRange extends RangeT
case class Range(start: Int, end:Int) extends RangeT
sealed trait LeftlongT
case object NoLeftLong extends LeftlongT
case class LeftLong(range: RangeT) extends LeftlongT

given semiringLeftlong: Semiring[LeftlongT] with {
  def zero =  NoLeftLong 
  def one = LeftLong(NoRange) 

  def plus(a: LeftlongT, b: LeftlongT): LeftlongT = (a,b) match{
    case (NoLeftLong, x) => x
    case (x, NoLeftLong) => x
    case (LeftLong(x), LeftLong(y)) => LeftLong(leftlong(x, y))
  } 
  def times(a: LeftlongT, b: LeftlongT): LeftlongT = (a,b) match {
        case (NoLeftLong, _) => NoLeftLong
        case (_, NoLeftLong) => NoLeftLong
        case (LeftLong(x), LeftLong(y)) =>  LeftLong(range(x, y))
  }
       
/* p.8: we pick the longer leftmost match rather than only considering the start position
|i <k ∨ i=k ∧j l= Range i j
|otherwise= Range k l */
  def leftlong(x: RangeT,y:RangeT): RangeT = (x,y) match {
        case (NoRange, NoRange) => NoRange
        case (NoRange, Range(i,j)) => Range(i,j)
        case (Range(i,j),NoRange) => Range(i,j)
        case (Range(i,j), Range(k,l)) => {
            if(i<k || (i == k && j >= l)) Range(i,j) else Range(k,l) 
        }
    }
   /* 
p.8: we pick the start position of the first part and the end position of the second part
LeftLong x ⊗LeftLong y= LeftLong (range x y)
where range... range (Range i ) (Range j) = Range i j */ 
  def range(x: RangeT,y:RangeT): RangeT = {(x,y) match {
        case (NoRange, NoRange) => NoRange
        case (NoRange, range) => range
        case (range, NoRange) => range
        case (Range(i,_), Range(_,j)) => 
            Range(i,j)
        }
    }
}
//instance Semiringi LeftLong where index i= LeftLong (Range i i)
given semiringILeftlong: SemiringI[LeftlongT] with {
    export semiringLeftlong.*// Inherit all `Semiring` operations
    def index(i:Int): LeftlongT = LeftLong(Range(i,i))
}


def mat[C,S](r: REG[C,S], s: List[C])(using semiring: SemiringI[S]) : REG[C,S] = s match {
  case Nil => r
  case c::cs => mat(shift(semiring.zero, r, c), cs)
}

def matcher[C,S](r: REG[C,S], s: List[C])(using semiring: SemiringI[S]) : S =
  if (s == Nil) nullable(r) 
    else fin(mat(r, s))

def submatcher[C, S](r: REG[C, S], s: List[C])(using semiring: SemiringI[S]): S = {
    val arb :REG[C,S] = intern(STAR(CHAR(_ => semiring.one)))
    val reg=BINIT(BSEQ(arb, BSEQ(r,arb)))
    matcher(reg, s)  
}

// internalises a regular expression into a marked
// regular expression
def intern[C,S](r: Rexp[C,S])(using semiring: SemiringI[S]) : REG[C,S] = r match {
  case ZERO() => BZERO()
  case ONE() => BONE()
  case CHAR(f) => BCHAR(semiring.zero, f)
  case ALT(r1, r2) => BALT(intern(r1), intern(r2))
  case SEQ(r1, r2) => BSEQ(intern(r1), intern(r2))
  case STAR(r) => BSTAR(intern(r))
  case NTIMES(r, n) => BNTIMES(intern(r),n)
}
// make sure the outermost REG is marked
def intern2[C,S](r: Rexp[C,S])(using semiring: SemiringI[S]) : REG[C,S] = BINIT(intern(r))

def helperf[S](c: Char)(using semiring: SemiringI[S]): Char => S = 
  (x: Char) => if (x == c) semiring.one else semiring.zero

def helperft[S](c: Char)(using semiring: SemiringI[S]): ((Char, Int)) => S = 
  (x: Char, pos: Int) => if (x == c) semiring.index(pos) else semiring.zero
 
@main
def test0() = {
    val br1 = SEQ(CHAR(helperf('a')), SEQ(CHAR(helperf('b')), CHAR(helperf('c'))))
    val br2 = intern2(br1)
    val s = "abc".toList
    println(matcher(br2,s))
    
}

@main
def test01() = {

    val a = CHAR(helperft('a'))
    val b = CHAR(helperft('b'))
    val rexp =NTIMES(SEQ(a, b) , 0)
    val regInit=intern2(rexp)

    val str = "ab abab".toList.zipWithIndex
    println("matcher : reg2 ")
    println(matcher(regInit, str))

    val reg= intern(rexp)

    println("submatcher : reg2 ")
    println(submatcher(reg, str))

}



def EVIL1(n: Int) = 
  SEQ(NTIMES(OPT(CHAR(helperft('a'))), n), NTIMES(CHAR(helperft('a')), n))

val EVIL2 = SEQ(STAR(STAR(CHAR(helperft('a')))), CHAR(helperft('b')))

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

//@arg(doc = "Test (a?{n}) (a{n})")
@main
def test1() = {
  for (i <- 0 to 8000 by 1000) {
    println(f"$i: ${time_needed(2, matcher(intern2(EVIL1(i)), ("a" * i ).toList.zipWithIndex))}%.5f")
  }
}

@main
def test2() = {
  for (i <- 0 to 5500000 by 500000) {
    println(f"$i: ${time_needed(2, matcher(intern2(EVIL2), ("a" * i).toList.zipWithIndex))}%.5f")
  }
} 
@main
def all() = { test1(); test2() } 

/*
def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

// strings are coerced into Rexps
given Conversion[String, Rexp[C,S]] = s => charlist2rexp(s.toList)
//val ABCD : Rexp = "abcd"

extension (r: Rexp[C,S]) {
  def | (s: Rexp[C,S]) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp[C,S]) = SEQ(r, s)
}

def string(r: Rexp[C,S]) : String = r match {
  case ZERO => "0"
  case ONE => "1"
  case CHAR(c) => c.toString 
  case ALT(r1, r2) => s"(${string(r1)} + ${string(r2)})"
  case SEQ(CHAR(c), CHAR(d)) => s"${c}${d}"
  case SEQ(r1, r2) => s"(${string(r1)} ~ ${string(r2)})"
  case STAR(r) => s"(${string(r)})*"
}

*/