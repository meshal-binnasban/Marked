import $file.rexp, rexp._
import scala.language.implicitConversions


// CDATA describes the constructors and 
// how many arguments they take

type CDATA[T] = List[(Int, List[T] => T)]


// the function digits takes a BigInt and generates a 
// list of min-digits according to a base of a number n
//
//  1 -> List(0, 0, 1)  for base 2 and 3 digits
//  2 -> List(0, 1, 0)
//  3 -> List(0, 1, 1)
//  ...
def digits(base: BigInt, minLen: Int)(n: BigInt): List[BigInt] = {
  def f(n: BigInt, len: Int): List[BigInt] = {
    if (n == 0) List.fill((minLen - len).max(0).intValue)(0)
    else f(n / base, len + 1) :+ (n % base)
  }
  f(n, 0)
}

// the function nh calculates the total number of trees 
// upto a certain height
//
//  height(1): nh => 5
//  height(2): nh => 60
//  height(3): nh => 7265
//
//       ... for regexes ZERO, ONE, CHAR, ALT, SEQ, STAR
//           and alphabet {a,b,c}

def nh[T](h: Int)(using cdata: CDATA[T]): List[BigInt] = {
  val tl = total(h - 1)
  cdata.scanLeft(BigInt(0))((a, b) => a + tl.pow(b._1))
}  

def total[T](h: Int)(using cdata: CDATA[T]): BigInt = 
  if (h == 0) 0 else nh(h).last

def get_height[T](n: BigInt, h: Int = 1)(using cdata: CDATA[T]) : Int = {
  if (n < total(h)) h else get_height(n, h + 1) 
} 

// decoding of a number i given a height d
def dec[T](d: Int, i: BigInt)(using cdata: CDATA[T]): T = {
    val groups = nh(d)
    val index = groups.indexWhere(i < _) - 1
    val node = cdata(index)
    val j = i - groups(index) // the index of the children
    val ds = digits(total(d - 1), node._1)
    node._2(ds(j).map(dec(d - 1, _)))
}

// main decoding function
def decode[T](n: BigInt)(using cdata: CDATA[T]) : T = {
  val h = get_height(n)
  dec(h, n)
}


// enumerates and prints out the first 1000 "usual" 
// regexes without STARs
@main
def test00() = {
  given rexp_cdata : CDATA[Rexp] = List(
        (0, _ => ONE),
        (0, _ => ZERO),
        (0, _ => CHAR('a')),
        (0, _ => CHAR('b')),
        (0, _ => CHAR('c')),
        (2, cs => ALT(cs(0), cs(1))),
        (2, cs => SEQ(cs(0), cs(1)))
      )

  for (i <- 0 to 1000) {
    println(s"$i: ${decode(i)}")
    println(decode(i).getClass)
  }
}  




// enumerates and prints out the first 1000 "usual" 
// regexes
@main
def test1() = {
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

  for (i <- 0 to 1000) {
    println(s"$i: ${decode(i)}")
  }
}  


// enumerates and prints out the first 10000 regexes 
// including NOT-regexes
@main
def test2() = {
  given rexp_cdata : CDATA[Rexp] = List(
        (0, _ => ONE),
        (0, _ => ZERO),
        (0, _ => CHAR('a')),
        (0, _ => CHAR('b')),
        (0, _ => CHAR('c')),
        (1, cs => STAR(cs(0))),
        (2, cs => ALT(cs(0), cs(1))),
        (2, cs => SEQ(cs(0), cs(1))),
        (1, cs => NOT(cs(0)))
      )

  for (i <- 0 to 10000) {
    println(s"$i: ${decode(i)}")
  }
} 


// enumerates 10 Mio regexes and prints out
// a star for each 10,000 enumerated regexes
@main
def test3() = {
  given rexp_cdata : CDATA[Rexp] = List(
        (0, _ => ONE),
        (0, _ => ZERO),
        (0, _ => CHAR('a')),
        (0, _ => CHAR('b')),
        (0, _ => CHAR('c')),
        (1, cs => STAR(cs(0))),
        (2, cs => ALT(cs(0), cs(1))),
        (2, cs => SEQ(cs(0), cs(1))),
        (1, cs => NOT(cs(0)))
      )

  for (i <- 0L to 10_000_000L) {
    val d = decode(i)
    if i % 10_000 == 0 then print("*")
    ()
  }
} 



// same as test1 but enumerates them in "parallel"

//import $cp.`scala-parallel-collections_3-1.0.4.jar` 
import $cp.`scala-parallel-collections_3-1.2.0.jar` 
   // needs this jar in the same path
import scala.collection.parallel.CollectionConverters._
import scala.util._

@main
def test4() = {

  given rexp_cdata : CDATA[Rexp] = List(
        (0, _ => ONE),
        (0, _ => ZERO),
        (0, _ => CHAR('a')),
        (0, _ => CHAR('b')),
        (0, _ => CHAR('c')),
        (1, cs => STAR(cs(0))),
        (2, cs => ALT(cs(0), cs(1))),
        (2, cs => SEQ(cs(0), cs(1))),
        (1, cs => NOT(cs(0)))
      )

  for (i <- (0L to 1000L).par) {
     println(s"$i: ${decode(i)}")
  }
}


@main
def test5() = {
  given rexp_cdata : CDATA[Rexp] = List(
        (0, _ => ONE),
        (0, _ => ZERO),
        (0, _ => CHAR('a')),
        (0, _ => CHAR('b')),
        (0, _ => CHAR('c')),
        (1, cs => STAR(cs(0))),
        (2, cs => ALT(cs(0), cs(1))),
        (2, cs => SEQ(cs(0), cs(1))),
        (1, cs => NOT(cs(0)))
      )

  for (i <- (0L to 10_000_000L).par) {
    val d = decode(i)
    if i % 10_000 == 0 then print("*")
    ()
  }
} 
