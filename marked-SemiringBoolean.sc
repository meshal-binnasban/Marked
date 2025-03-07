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
  case BNTIMES(r: REG[C,S] , n:Int , current:Int)
  case BINIT(r: REG[C,S])

  override def toString: String = this match {
    case BZERO() => "BZERO"
    case BONE() => "BONE"
    case BCHAR(b, _) => s"BCHAR($b)"
    case BALT(r1, r2) => s"BALT($r1, $r2)"
    case BSEQ(r1, r2) => s"BSEQ($r1, $r2)"
    case BSTAR(r) => s"BSTAR($r)"
    case BNTIMES(r, n, current) => s"BNTIMES($r, n=$n, current=$current)"
    case BINIT(r) => s"BINIT($r)"
  }

}
import REG._
//BNTIMES() repeat with limit n, check if n+1 then stop?


def nullable[C,S](r: REG[C,S])(using semiring: Semiring[S]) : S = r match {
  case BZERO() => semiring.zero
  case BONE()=> semiring.one
  case BCHAR(_,f) =>  semiring.zero
  case BALT(r1, r2) => semiring.plus(nullable(r1),nullable(r2))
  case BSEQ(r1, r2) => semiring.times(nullable(r1),nullable(r2))
  case BSTAR(r) => semiring.one
  case BNTIMES(r, n , current) => if (n == 0) semiring.one else nullable(r)
  case BINIT(r) => nullable(r)
}

def fin[C,S](r: REG[C,S])(using semiring: Semiring[S]) : S = r match {
  case BZERO() => semiring.zero
  case BONE() => semiring.zero
  case BCHAR(b,_) => b
  case BALT(r1, r2) => semiring.plus( fin(r1) , fin(r2) )
  case BSEQ(r1, r2) =>semiring.plus(semiring.times(fin(r1),nullable(r2)),fin(r2))
  case BSTAR(r) => fin(r)
  case BNTIMES(r, n, current) => 
    if (n == 0) nullable(r)  
    else if (current == n || (nullable(r) == semiring.one) && current<n) fin(r)
    else semiring.zero  
}
def shift[C,S](mark: S, re: REG[C,S], c: C)(using semiring: Semiring[S]): REG[C,S] = {
    re match {
      case BZERO() => BZERO() // or just re 
      case BONE() => BONE() // or just re 
      case BCHAR(b,f) => BCHAR(semiring.times(mark, f(c)), f) 
      case BALT(r1, r2) => BALT(shift(mark, r1, c), shift(mark, r2, c))
      case BSEQ(r1, r2) =>
        BSEQ(shift(mark, r1, c),
             shift(semiring.plus(semiring.times(mark, nullable(r1)), fin(r1)), r2, c))
      case BSTAR(r) => BSTAR(shift(semiring.plus(mark, fin(r)), r, c))
      case BNTIMES(r, n , current) => 
        if (n==0) {
            BONE()
        } else {

            // this is not yet working properly, when testing the evil reg gives wrong result
             if(current < n+1)
                {
                    val rr= shift(semiring.plus(mark, fin(r)), r , c)
                    if(fin(rr) == semiring.one){
                    BNTIMES(rr, n, current + 1)
                    }
                    else
                        BNTIMES(rr, n, current )
                }else{ 
                println(s" re is  $re")
                re
                }
        }

          
          /*
           if (n == 0) {
            BONE()
            } else { 
                if (current < n+1) {
                    val rr = shift(semiring.plus(mark, fin(r)), r, c)
                    //|| nullable(r) == semiring.one
                    if (fin(r) == semiring.one || nullable(r) == semiring.one) {
                        BNTIMES(rr, n, current + 1)
                    } else {
                        BNTIMES(rr, n, current)
                    }
                } else {
                    BNTIMES(r, n, current) // Maintain the correct state
                }
            }
       */
       /*
        if (n == 0) {
            BONE()
        } else { 
            if(current < n){
            val rr = shift(semiring.plus(mark, fin(r)), r, c)
                if (fin(r) == semiring.one) {
                    BNTIMES(rr, n, current + 1)
                } else {
                    BNTIMES(rr, n, current)
                }
        }
        else {
            if(current == n){
                re
            }else BZERO()
            
        }
        }
     */   
      /*  
        if (current >= n) {
            BONE()
        } else {
            val rr = shift(semiring.plus(mark, fin(r)), r, c)
            if (fin(rr) == semiring.one) {
                BNTIMES(rr, n, current + 1)
            } else {
                BNTIMES(rr, n, current)
            }
        }
         */
       /* 
        if(n==0){
            BONE()
        }
        else
        { 
            if(current < n+1){
                val rr=shift(semiring.plus(mark,fin(r)),r,c)
                if(fin(rr) == semiring.one){
                    val x=BNTIMES(rr,n,current+1)
                    println(s"this is x $x")
                    x
                }else
                    BNTIMES(rr,n,current)
                }
                else
                {
                    re
                }
        }
        */ 
      case BINIT(r) => shift(semiring.one, r, c)
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


def mat[C,S](r: REG[C,S], s: List[C])(using semiring: Semiring[S]) : REG[C,S] = s match {
  case Nil => r
  case c::cs => mat(shift(semiring.zero, r, c), cs)
}

def matcher[C,S](r: REG[C,S], s: List[C])(using semiring: Semiring[S]) : S =
  if (s == Nil) nullable(r) 
    else { //fin(mat(r, s))
        val x=mat(r, s)
        val y = fin(x)
        println(s" last reg is $x and fin($y)")
        y
     }

def submatcher[C, S](r: REG[C, S], s: List[C])(using semiring: Semiring[S]): S = {
    val arb :REG[C,S] = intern(STAR(CHAR(_ => semiring.one)))
    val reg=BINIT(BSEQ(arb, BSEQ(r,arb)))
    matcher(reg, s)  
}

// internalises a regular expression into a marked
// regular expression
def intern[C,S](r: Rexp[C,S])(using semiring: Semiring[S]) : REG[C,S] = r match {
  case ZERO() => BZERO()
  case ONE() => BONE()
  case CHAR(f) => BCHAR(semiring.zero, f)
  case ALT(r1, r2) => BALT(intern(r1), intern(r2))
  case SEQ(r1, r2) => BSEQ(intern(r1), intern(r2))
  case STAR(r) => BSTAR(intern(r))
  case NTIMES(r, n) => BNTIMES(intern(r),n,current=0)
}
// make sure the outermost REG is marked
def intern2[C,S](r: Rexp[C,S])(using semiring: Semiring[S]) : REG[C,S] = BINIT(intern(r))

def helperf[S](c: Char)(using semiring: Semiring[S]): Char => S = 
  (x: Char) => if (x == c) semiring.one else semiring.zero

/*
def helperft[S](c: Char)(using semiring: Semiring[S]): ((Char, Int)) => S = 
  (x: Char, pos: Int) => if (x == c) semiring.index(pos) else semiring.zero
 */

@main
def test0() = {
    val br1 = SEQ(CHAR(helperf('a')), SEQ(CHAR(helperf('b')), CHAR(helperf('c'))))
    val br2 = intern2(br1)
    val s = "abc".toList
    println(matcher(br2,s))  
}

def myNtimes[C, S](r: REG[C, S]): REG[C, S] = r match {
    case BNTIMES(r, n, _) if n <= 0 => BONE() 
    case BNTIMES(r, 1, _) => r 
    case BNTIMES(r, n, _) => BSEQ(r, myNtimes(BNTIMES(r, n - 1, 0)))
    case _ => r 
}

@main
def test01() = {
/*
   val n=4
    val a = CHAR(helperf('a'))
    val b = CHAR(helperf('b'))
    val rexp =NTIMES(ALT(a,ONE()), n) // one causes issues

    val testSequences=BINIT(myNtimes(intern(rexp)))
    val regInit=intern2(rexp)
    val str = "ab".toList
    println(s"input is  : str= $str and n is $n , first is NTIMES constructed as SEQs, Second is using the constructor NTIMES")
    println(matcher(testSequences, str))
    println("\n =========== \n")
    println(matcher(regInit, str))
*/
    //val reg= intern(rexp)
    //println("submatcher : reg ")
    //println(submatcher(reg, str))
    
    println("\n +++++++++++++++ \n")
    val s = "aaa".toList
    val interReg=intern2(EVIL1(1))
   // println(s"NTIMES, reg is $interReg")
    println(matcher(interReg,s))
    println("\n Converted to Seq \n")
    val evilrxp=SEQ(SEQ(ALT(CHAR(helperf('a')),ONE()),ALT(CHAR(helperf('a')),ONE())) ,  SEQ(CHAR(helperf('a')), CHAR(helperf('a')) ))
    println(matcher(intern2(evilrxp),s))
    

}

def EVIL1(n: Int) = 
  SEQ(NTIMES(OPT(CHAR(helperf('a'))), n), NTIMES(CHAR(helperf('a')), n))

val EVIL2 = SEQ(STAR(STAR(CHAR(helperf('a')))), CHAR(helperf('b')))

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
    println(f"$i: ${time_needed(2, matcher(intern2(EVIL1(i)), ("a" * i ).toList))}%.5f")
  }
}

@main
def test2() = {
  for (i <- 0 to 10000000 by 500000) {
    println(f"$i: ${time_needed(2, matcher(intern2(EVIL2), ("a" * i).toList))}%.5f")
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