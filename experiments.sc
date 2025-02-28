
enum Rexp {
  case ZERO
  case ONE 
  case CHAR(c: Char )
  case ALT(r1: Rexp, r2: Rexp )
  case SEQ(r1: Rexp, r2: Rexp)
  case STAR(r: Rexp)
//  case NTIMES(r: Rexp, n: Int) // from re4.sc
}
import Rexp._

enum Rexpw[C, S]{
  case ZEROw extends Rexpw[Nothing, Nothing]
  case ONEw extends Rexpw[Nothing, Nothing]
  case CHARw(f: C => S , val finv:S) 
  case ALTw(r1: Rexpw[C, S], r2: Rexpw[C, S])
  case SEQw(r1: Rexpw[C, S], r2: Rexpw[C, S])
  case STARw(r: Rexpw[C, S])
 // case NTIMESw(r: Rexpw[C, S], n: Int)
}
import Rexpw._

def CHARb[S](c: Char)(using semiring: Semiring[S]): Rexpw[Char, S] =
  CHARw(x => if (x == c ) semiring.one else semiring.zero , semiring.zero)

def CHARi[S: SemiringI](c: Char): Rexpw[(Int, Char), S] = {
    val sri = summon[SemiringI[S]]
    val sr = summon[Semiring[S]]

    def weight(t: (Int, Char)): S = {
      val (pos, x) = t
      if (x == c) sri.index(pos)
      else sr.zero
    }
    CHARw(weight,sr.zero)
  }



def weighted[S](r: Rexp)(using semiring: Semiring[S]): Rexpw[Char, S]  = r match {
      case ZERO      => ZEROw.asInstanceOf[Rexpw[Char,S]]
      case ONE       => ONEw.asInstanceOf[Rexpw[Char,S]]
      case CHAR(c)    => CHARb(c)
      case ALT(r1,r2)  => ALTw(weighted(r1), weighted(r2))
      case SEQ(r1,r2)  => SEQw(weighted(r1), weighted(r2))
      case STAR(r1)    => STARw(weighted(r1))
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

given semiringInt: Semiring[Int] with {
    def zero = 0
    def one = 1
    def plus(a: Int, b: Int): Int = a + b
    def times(a: Int, b: Int): Int = a * b
  }

trait SemiringI[S] extends Semiring[S] {
    def index(i: Int): S
  }



// maybe change to old? since this is not related to counting for example?
def nullable[C, S](r: Rexpw[C, S])(using semiring: Semiring[S]): S = r match {
  case ZEROw => semiring.zero
  case ONEw => semiring.one
  case CHARw(_,finv) => semiring.zero
  case ALTw(r1, r2) => semiring.plus(nullable(r1), nullable(r2))
  case SEQw(r1, r2) => semiring.times(nullable(r1), nullable(r2))
  case STARw(_) => semiring.one
//  case NTIMESw(r, i) => if (i == 0) semiring.one else nullable(r)
}

/*
def nullable[C,S](r: Rexpw[C,S]) : Boolean = r match {
  case ZEROw => false
  case ONEw => true
  case CHARw(_) => false
  case ALTw(r1, r2) => nullable(r1) || nullable(r2)
  case SEQw(r1, r2) => nullable(r1) && nullable(r2)
  case STARw(_) => true
  case NTIMESw(r, i) => if (i == 0) true else nullable(r) //?
}
*/
// from re1.sc
//def OPT(r: Rexp[C,S]) = ALT(r, ONE)
def fin[C, S](r: Rexpw[C, S])(using semiring: Semiring[S]): S = r match {
  case ZEROw => semiring.zero
  case ONEw => semiring.zero
  case CHARw(f,finv) => finv //???? or f(_) but!!
  case ALTw(r1, r2) => semiring.plus(fin(r1),fin(r2))
  case SEQw(r1, r2) => 
    semiring.plus(semiring.times(fin(r1), nullable(r2)), fin(r2))
  case STARw(r) => fin(r)
//  case NTIMESw(r, i) =>  fin(r)// ? if (i == 0) false else
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
def shift[C,S](mark: S, r: Rexpw[C,S], c: C)(using semiring: Semiring[S]): Rexpw[C,S] = r match {
    case ZEROw => ZEROw
    case ONEw=> ONEw
    case CHARw(f,finv) =>  CHARw(f,semiring.times(mark, f(c))) // must add final variable for it to change when called in final function or?
    //in article: shiftw m (SYMw f) c = (symw f) {finalw = m ⊗f c}
    case ALTw(r1, r2) => ALTw(shift(mark,r1,c),shift(mark,r2,c))
    case SEQw(r1,r2) => 
      SEQw(shift(mark,r1,c), 
          shift( semiring.plus
                              (semiring.times(mark , nullable(r1))  , fin(r1)) ,  r2,c))
    case STARw(r) => STARw(shift(semiring.plus(mark , fin(r)), r,c))
   
   /*
    case NTIMES(r, n) => 
      if(n==0) ONE[C,S]
      else SEQ(shift(semiring.plus(mark , fin(r)), r,c), NTIMES(r,n-1)) 
      */
}

def matcher[C, S](r: Rexpw[C, S], s: List[C])(using semiring: Semiring[S]): S = s match {
case Nil => nullable(r)
case c :: cs => fin(cs.foldLeft(shift(semiring.one, r, c))(shift(semiring.zero, _, _)))

//case c :: s => fin(s.foldLeft(shift(true, weighted(r), c)) { (acc, c) =>
//  shift(false, acc, c) })
}

@main
def test1() = {
val r = STAR(SEQ(SEQ(CHAR('a'), CHAR('b')) , STAR(CHAR('c'))) )
println(r)
val rw=weighted(r)(using booleanSemiring)
println("testing new weighted Marked")
println(matcher(rw, "abcabcd".toList)(using booleanSemiring))

//val r= NTIMES(CHAR('b'),2)
//matcher(r, "bbc".toList)

}

/*
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


*/


