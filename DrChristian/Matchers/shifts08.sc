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

type Marks = Set[(Int, Int)]

def snds(ms: Marks) = ms.map((_, m) => (m, m))

def advance(ms: Marks, subs: Set[String], s: String) : Marks = {
    val ms1 = (for ((n, m) <- ms; ss <- subs;
                if m + ss.length <= s.length;
                if s.substring(m, m + ss.length) == ss) yield (n, m + ss.length))
    if (ms1 == Set()) Set() else ms1 ++ advance(ms1, subs, s)            
}

// shifts function 
def shifts(ms: Marks, s: String, r: Rexp) : Marks = r match {
  case ZERO => Set()
  case ONE => Set()
  case CHAR(c) => for ((n, m) <- ms; if m < s.length && s(m) == c) yield (n, m + 1)
  case ALT(r1, r2) => shifts(ms, s, r1) ++ shifts(ms, s, r2)
  case SEQ(r1, r2) => {
    val ms1 = shifts(ms, s, r1)
    (nullable(r1), nullable(r2)) match {
      case (true, true) =>  shifts(ms1 ++ ms, s, r2) ++ ms1
      case (true, false) => shifts(ms1 ++ ms, s, r2) 
      case (false, true) => shifts(ms1, s, r2) ++ ms1
      case (false, false) => shifts(ms1, s, r2)
    }   
  }
  case STAR(r) => {
    val ms1 = shifts(snds(ms), s, r)
    println(s"ms1=$ms1")
    val subs = ms1.map((n, m) => s.substring(n, m))
    val v= ms1++advance(ms1, subs, s)
    println(s"v=$v,advance=${advance(ms1, subs, s)}, subs=$subs")
    v
  }
  case NTIMES(r, n) =>
    if (n == 0) Set()               // or return ms?
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
def mat(r: Rexp, s: String) : Marks = 
  shifts(Set((0, 0)), s, r)

def matcher(r: Rexp, s: String) : Boolean = {
  if (s == "") nullable(r)
  else
    val ms=mat(r,s)
    println(s"ms=$ms ,\n s.length=${s.length}")

    mat(r, s).exists(_._2 == s.length)
}



@main
def testall() = {
  given rexp_cdata : CDATA[Rexp] = List(
        (0, _ => ONE),
        (0, _ => ZERO),
        (0, _ => CHAR('a')),
        (0, _ => CHAR('b')),
        (0, _ => CHAR('c')),
        (1, cs => STAR(cs(0))),
        (2, cs => ALT(cs(0), cs(1))),
        (2, cs => SEQ(cs(0), cs(1)))
      )
  val alphabet = LazyList('a', 'b')

  for (i <- 0L to 100_000_000L) {
    val r = decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- (generate_up_to(alphabet)(20)(r).take(19)) if s != "") {
      val res = matcher(r, s)
      if (!res) {
        println(s"$r and $s")
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

@main
def test1() = {
   val reg = STAR(mkalts(5)) 
   for (n <- (0 to 1000 by 100)) {
      println(s"$n ${time_needed(2, matcher(reg, "a" * n))}")
   }
}

@main
def test2() = {
  val reg = STAR("a")
  for (n <- 1 to 5) {
    val s = "a" * n 
    println(s"$n, $s: ${matcher(reg, s)}  ${mat(reg, s)} ")
  }
}


@main
def test3() = {
  val reg = NTIMES(("a" | ONE), 3)
  for (n <- 0 to 5) {
    val s = "a" * n 
    println(s"$n, $s: ${matcher(reg, s)}")
  }
}

@main
def test4() = {
  val reg = (%("a"|"b") )
    val s = "ab"
    println(s" $s: ${matcher(reg, s)}")
    println(s"$s: ${matcher(reg, s)}  ${mat(reg, s)} ")

    println(s"mkalt=${mkalts(3)}")
    println(s"snd=${snds(Set((0,1), (0,3)))}")
}

@main
def test5() = {
  val reg = ("a" ~ "b" )
    val s = "ab"
    println(s" $s: ${matcher(reg, s)}")
    println(s"$s: ${matcher(reg, s)}  ${mat(reg, s)} ")
}
