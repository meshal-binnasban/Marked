error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/shifts08.sc:iterator.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/shifts08.sc
empty definition using pc, found symbol in pc: iterator.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -rexp/ks/iterator.
	 -enumerate/ks/iterator.
	 -regenerate/ks/iterator.
	 -ks/iterator.
	 -scala/Predef.ks.iterator.
offset: 5337
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/shifts08.sc
text:
```scala
// simple test using SETS instead of list of marks
//
// => 26 mins (for 100_000_000)
// => not much change from the list version for 10 strings
//   
// 53 mins vs 85 mins for 20 strings and 100_000_000

import scala.language.implicitConversions
/* //> using file rexp.sc 
//> using file enumerate.sc
//> using file regenerate.sc */
import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.rebit


case class Mark(n: Int, m: Int)

type Marks = Set[Mark]

var splits: Map[Int, Set[Int]] = Map.empty

def recordSplit(id: Int, k: Int): Unit =
  splits = splits.updated(id, splits.getOrElse(id, Set.empty) + k)

// shifts function 
def shifts(ms: Marks, s: String, r: Rexp) : Marks = r match {
  case ZERO => Set()
  case ONE => Set()
  case CHAR(c) =>
     for { Mark(n, m) <- ms if m < s.length && s(m) == c } yield Mark(n, m + 1)
  case ALT(r1, r2) => shifts(ms, s, r1) ++ shifts(ms, s, r2)
  case SEQS(r1, r2, id) =>
    val ms1 = shifts(ms, s, r1)
    val inR2 = if (nullable(r1)) ms1 ++ ms else ms1
    inR2.foreach(q => recordSplit(id, q.m))

    (nullable(r1), nullable(r2)) match {
      case (true,  true)  => shifts(ms1 ++ ms, s, r2) ++ ms1
      case (true,  false) => shifts(ms1 ++ ms, s, r2)
      case (false, true)  => ms1 ++ shifts(ms1, s, r2)
      case (false, false) => shifts(ms1, s, r2)
    }

  case STARSS(r, id) =>
    val ms1 = shifts(ms, s, r)
    ms1.foreach(q => recordSplit(id, q.m))

    if (ms1.isEmpty) Set()
    else ms1 ++ shifts(ms1, s, STARSS(r, id))
      
  case NTIMES(r, n) =>
    if (n == 0) Set()              
    else if (n == 1) shifts(ms, s, r)
    else {
      val ms1 = shifts(ms, s, r)
      if (ms1 == Set()) Set()
      else
      if (nullable(r)) ms1 ++ shifts(ms1, s, NTIMES(r, n - 1))
      else shifts(ms1, s, NTIMES(r, n - 1))
    }
}

// the main matching function 
def mat(r: Rexp, s: String): Marks = {
  splits = Map.empty
  shifts(Set(Mark(0,0)), s, r)
}

def matcher(r: Rexp, s: String) : Boolean = {
  if (s == "") nullable(r)
  else
    val ms=mat(r,s)
    mat(r, s).exists(_._2 == s.length)
}

def lexer(r: Rexp, s: String): Val = {
  if (s == "") {
    if (nullable(r)) Empty else Invalid
  } else {
    val ms     = mat(r, s)
    val finals = ms.filter(_.m == s.length)
    if (finals.isEmpty) Invalid
    else {
      val best: Mark = finals.head
      //println(splits)
      val (v, p0) = back(r, s, best,splits = splits)
      if (isInvalid(p0)) Invalid else v
    }
  }
}

def isInvalid(q: Mark): Boolean = q.n < 0

val invalid = (Invalid, Mark(-1, -1))

def back(r: Rexp, s: String, p: Mark, splits: Map[Int, Set[Int]]): (Val, Mark) = r match {

  case ONE =>
    val Mark(n, m) = p
    if (n == m) (Empty, p) else invalid

  case CHAR(c) =>
    val Mark(n, m) = p
    if (m == n + 1 && m <= s.length && s(n) == c) (Chr(c), Mark(n, m - 1))
    else invalid

  case ALT(r1, r2) =>
    //println(s"ALT: p=$p")
    val (vL, pL) = back(r1, s, p, splits)
    val (vR, pR) = back(r2, s, p, splits)
    //println(s"ALT:  pL=$pL;  pR=$pR")
    val mm = p.m
    def long(q: Mark) = if (isInvalid(q)) -1 else mm - q.m
    val dL = long(pL)
    val dR = long(pR)
    //println(s"ALT: dL=$dL, dR=$dR")
    if (dL < 0 && dR < 0) invalid
    else if (dL > dR) (Left(vL),  pL)
    else if (dR > dL) (Right(vR), pR)
    else              (Left(vL),  pL)

  case SEQS(r1, r2, id) =>
    val Mark(n, m) = p
    val r1All =
      if (!nullable(r2)) invalid
      else back(r1, s, Mark(n, m), splits) match {
        case (v1, p1) if !isInvalid(p1) => (Sequ(v1, mkeps(r2)), p1)
        case _ => invalid
      }

    val kSplits = filterSplits(splits.getOrElse(id, Set.empty), p, nullable(r1), nullable(r2))  
    
    val r1r2: (Val, Mark) = kSplits.map { 
      k => back(r1, s, Mark(n, k), splits) match {
            case (v1, p1) if !isInvalid(p1) =>
              println(s"SEQS: k=$k, v1=$v1 p1=$p1") 
              back(r2, s, Mark(k, m), splits) match {
                case (v2, p2) if !isInvalid(p2) => 
                  println(s"SEQS: k=$k, v1=$v1, v2=$v2")
                  (Sequ(v1, v2), p1)
                case _ => invalid
              }
            case _ => invalid
          }
        }.collectFirst { case result if !isInvalid(result._2) => result }.getOrElse(invalid)

    val r2All = if (!nullable(r1)) invalid
                else back(r2, s, Mark(n, m), splits) match {
                     case (v2, p2) if !isInvalid(p2) => (Sequ(mkeps(r1), v2), p2) 
                     case _ => invalid
                     }

    (nullable(r1), nullable(r2)) match {
      case (true,  true)  => if (!isInvalid(r1All._2)) r1All else if (!isInvalid(r1r2._2)) r1r2 else r2All
      case (true,  false) => if (!isInvalid(r1r2._2)) r1r2 else r2All
      case (false, true)  => if (!isInvalid(r1All._2)) r1All else r1r2
      case (false, false) => r1r2
    }
    
  case STARSS(r, id) =>
    val Mark(n, m) = p

    def decodeStar(a: Int, b: Int): Option[List[Val]] =
      if (a == b) Some(Nil)
      else {
        val ks = splits.getOrElse(id, Set.empty).filter(k => k > a && k <= b)
        .toList.sorted(Ordering.Int.reverse)
        ks.ite@@rator.map { k =>
            val (v1, p1) = back(r, s, Mark(a, k), splits)
            if (isInvalid(p1)) None
            else {
              decodeStar(k, b) match {
                case Some(vs) => Some(v1 :: vs)
                case None     => None
              }
            }
          }
          .collectFirst { case Some(vs) => vs }
      }

    decodeStar(n, m) match {
      case Some(vs) => (Stars(vs), Mark(n, n))
      case None     => invalid
    }
      
      
  case _ => invalid
}

def filterSplits( initSplits: Set[Int],p: Mark, nullableR1: Boolean, nullableR2: Boolean): Iterator[Int] = {
  val Mark(n, m) = p
  val inRange = initSplits.filter(k => k >= n && k <= m)
  val forR1 = if (nullableR1) inRange else inRange.filter(_ > n)
  val forR2 = if (nullableR2) forR1 else forR1.filter(_ < m)
  forR2.toList.sorted(Ordering.Int.reverse).iterator
}

@main
def test1() = {
  val reg = ("aa") | ("a"~ (ONE ~ "a")) 
  val s="aa"
  println(s"$s: r=\n${pp(reg)}  ${mat(reg, s)} ")

  println(lexer(reg,s))
  println(rebit.blexer(reg,s))

}
@main
def test2() = {
  val reg = ("a"~ ("a"|ONE))
  val s="aa"
  println(s"$s: r=\n${pp(reg)}  ")

  println(lexer(reg,s))
  println(rebit.blexer(reg,s))

}

@main
def test3() = {
  val reg = %("a" |"aa")
  val s="aaaa"
  println(s"$s: r=\n${pp(reg)}  ")

  val vmark=lexer(reg,s)
  val vder= rebit.blexer(reg,s)
  println(vmark)
  println(vder)
  println(vmark == vder)

}

@main
def testall() = {
  given rexp_cdata : CDATA[Rexp] = List(
        (0, _ => ONE),
        (0, _ => ZERO),
        (0, _ => CHAR('a')),
        (0, _ => CHAR('b')),
        (0, _ => CHAR('c')),
        (1, cs => mkSTARS(cs(0))),
        (2, cs => ALT(cs(0), cs(1))),
        (2, cs => mkSEQS(cs(0), cs(1)))
      )
  val alphabet = LazyList('a', 'b')

  for (i <- 0L to 1_000_000_000L) {
    val r = decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- (generate_up_to(alphabet)(20)(r).take(19)) if s != "") {
      
      val vm=lexer(r,s)
      val vb=rebit.blexer(r,s)
      val res = vm == vb
      if (!res) {
        println(s"$r and $s")
        println(s"vm=$vm vb=$vb")
        System.exit(1)
      }
    }
  }
}


// for measuring time
def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

def mkstar(n: Int) = STAR("a" * n)
def mkalts(n: Int) = {
  (for (i <- (1 to n).toList) yield mkstar(i)).reduceLeft(ALT.apply)
}


/* 
in the star case, we can maybe control it by our definition of posix. 
meaning that i want the inner r to consume the most at first, similar 
to seqs and then the next will be the next smallest splits and so on. 
what i want to avoid is iterating over all possible ways it could split. 
another way maybe to keep a set of the how much an iteration can consume, 
so let say that an iteration can conume 5,4,2. then in back we try the splits 
but that could be costly since we need to try the different combinations, i dont know.

    val (vMs1, pMs1) = back(r, s, p, splits)
    println(s"STAR: vMs1=$vMs1, pMs1=$pMs1")
    if(isInvalid(pMs1)) invalid
    else {
      val (Stars(vs), pMs2) = back(STAR(r), s, pMs1, splits)
      (Stars(vMs1::vs), pMs2)
    }
*/
```


#### Short summary: 

empty definition using pc, found symbol in pc: iterator.