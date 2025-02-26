error id: foldLeft.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-v1.sc
empty definition using pc, found symbol in pc: foldLeft.
semanticdb not found
|empty definition using fallback
non-local guesses:
	 -Rexp.cs.foldLeft.
	 -Rexp.cs.foldLeft#
	 -Rexp.cs.foldLeft().
	 -cs/foldLeft.
	 -cs/foldLeft#
	 -cs/foldLeft().
	 -scala/Predef.cs.foldLeft.
	 -scala/Predef.cs.foldLeft#
	 -scala/Predef.cs.foldLeft().

Document text:

```scala
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

// from re1.sc
def OPT(r: Rexp) = ALT(r, ONE)

def shift(mark: Boolean,r: Rexp,c: Char ): Rexp = r match {
    case ZERO => ZERO
    case ONE=> ONE
    case CHAR(ch,marked) => CHAR(ch, ch==c && mark)
    case ALT(r1, r2) => ALT(shift(mark,r1,c),shift(mark,r2,c))
    case SEQ(r1,r2) => SEQ (shift(mark,r1,c), shift(mark && nullable(r1) || fin(r1),r2,c))
    case STAR(r) => STAR(shift(mark || fin(r), r,c))
    case NTIMES(r, n) => 
      if(n==0) ONE 
      else SEQ(shift(mark || fin(r), r,c), NTIMES(r,n-1)) 
}

def nullable(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_,_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, i) => if (i == 0) true else nullable(r) //?
}

def fin(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => false
  case CHAR(c,marked) => marked
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
  case NTIMES(r, i) =>  fin(r)// ? if (i == 0) false else
}

def matcher(r: Rexp , s: String) : Boolean = s.toList match {
case Nil => nullable(r)
case c :: cs => fin(cs.foldLeft(shift(true, r, c))(shift(false, _, _)))
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

empty definition using pc, found symbol in pc: foldLeft.