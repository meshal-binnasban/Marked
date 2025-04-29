// A simple matcher for basic regular expressions
//
// Call the testcases with X = {1,2,3}
//
//   amm re1.sc testX
//
// or 
//
//   amm re1.sc all
//

 
// regular expressions
enum Rexp {
  case ZERO                     // matches nothing
  case ONE                      // matches an empty string
  case CHAR(c: Char)            // matches a character c
  case ALT(r1: Rexp, r2: Rexp)  // alternative
  case SEQ(r1: Rexp, r2: Rexp)  // sequence
  case STAR(r: Rexp)            // star
  case PLUS(r: Rexp)            // r+ (one or more repetitions)
}
import Rexp._
// the optional regular expression (one or zero times)
def OPT(r: Rexp) = ALT(r, ONE)   // r + 1

// the n-times regular expression (explicitly expanded to SEQs)
def NTIMES(r: Rexp, n: Int) : Rexp = n match {
  case 0 => ONE
  case 1 => r
  case n => SEQ(r, NTIMES(r, n - 1))
}

// nullable function: tests whether a regular 
// expression can recognise the empty string  
def nullable(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case PLUS(r) => nullable(r)
}

// the derivative of a regular expression w.r.t. a character
def der(c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => 
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r1) => SEQ(der(c, r1), STAR(r1))
  case PLUS(r1) => SEQ(der(c,r), PLUS(r)) 
}

// the derivative w.r.t. a string (iterates der)
def ders(s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, der(c, r))
}

def simp(r: Rexp) : Rexp = r match {
  case ALT(r1, r2) => (simp(r1), simp(r2)) match {
    case (ZERO, r2s) => r2s
    case (r1s, ZERO) => r1s
    case (r1s, r2s) => if (r1s == r2s) r1s else ALT (r1s, r2s)
  }
  case SEQ(r1, r2) =>  (simp(r1), simp(r2)) match {
    case (ZERO, _) => ZERO
    case (_, ZERO) => ZERO
    case (ONE, r2s) => r2s
    case (r1s, ONE) => r1s
    case (r1s, r2s) => SEQ(r1s, r2s)
  }
  case r => r
}

def ders2(s: List[Char], r: Rexp) : Rexp = (s, r) match {
  case (Nil, r) => r
  case (s , ZERO) => ZERO
  case (s , ONE) => if (s == Nil) ONE else ZERO
  case (s, CHAR(c)) => if (s == List(c)) ONE else 
                       if (s == Nil) CHAR(c) else ZERO
  case (s, ALT(r1, r2)) => ALT(ders2(s, r1), ders2(s, r2))
  case (c :: s, PLUS(r)) => ders2(s, simp(SEQ(der(c, r), PLUS(r))))
  case (c::s, r) => ders2(s, simp(der(c, r)))
  
}


// the main matcher function
def matcher(r: Rexp, s: String) : Boolean = 
  nullable(ders2(s.toList, r))

// some examples from the homework

val r = SEQ(CHAR('a'), CHAR('c'))
matcher(r, "ac")

val r1 = STAR(ALT(SEQ(CHAR('a'), CHAR('b')), CHAR('b')))
der('a', r)
der('b', r)
der('c', r)

val r2 = SEQ(SEQ(CHAR('x'), CHAR('y')), CHAR('z'))
der('x', r2)
der('y', der('x', r2))
der('z', der('y', der('x', r2)))


// Test Cases
//============
// the evil regular expression  (a?){n} a{n}
def EVIL1(n: Int) = 
  SEQ(NTIMES(OPT(CHAR('a')), n), NTIMES(CHAR('a'), n))

// the evil regular expression (a*)* b
val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

// for measuring time
def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}


// test: (a?{n}) (a{n})
@main
def test1() = {
  println("Test (a?{n}) (a{n})")

  for (i <- 0 to 20 by 2) {
    println(f"$i: ${time_needed(2, matcher(EVIL1(i), "a" * i))}%.5f")
  }
}

// test: (a*)* b
@main
def test2() = {
  println("Test (a*)* b")

  for (i <- 0 to 20 by 2) {
    println(f"$i: ${time_needed(2, matcher(EVIL2, "a" * i))}%.5f")
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
}

// the expicit expansion in EVIL1(n) increases
// drastically its size - (a?){n} a{n}

size(EVIL1(1))  // 5
size(EVIL1(3))  // 17
size(EVIL1(5))  // 29
size(EVIL1(7))  // 41
size(EVIL1(20)) // 119

size(ders(("a" * 20).toList, EVIL1(20))) 


// given a regular expression and building successive
// derivatives might result into bigger and bigger
// regular expressions...here is an example for this:


// (a + aa)*
val BIG = STAR(ALT(CHAR('a'), SEQ(CHAR('a'), CHAR('a'))))

size(ders2("".toList, BIG))              // 13
size(ders2("aa".toList, BIG))            // 51
size(ders2("aaaa".toList, BIG))          // 112
size(ders2("aaaaaa".toList, BIG))        // 191
size(ders2("aaaaaaaa".toList, BIG))      // 288
size(ders2("aaaaaaaaaa".toList, BIG))    // 403
size(ders2("aaaaaaaaaaaa".toList, BIG))  // 536


size(ders2(("a" * 30).toList, BIG))      // 31010539

@main
def test3() = {
  println("Test (a + aa)*")

  for (i <- 0 to 30 by 5) {
    println(f"$i: ${time_needed(2, matcher(BIG, "a" * i))}%.5f")
  }
}

@main
def test4() = {
  
}

@main
def all() = { test1(); test2() ; test3() } 




