import scala.language.implicitConversions
import $file.Shifts_Sets, Shifts_Sets._
import $file.rexp, rexp._
import $file.rebit
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._

def language(r: Rexp)(s: String): Boolean =
  rebit.matcher(r, s)

def strip(A: String => Boolean, s: String): Set[String] =
  (1 to s.length).collect {
    case k if A(s.substring(0, k)) => s.substring(k)
  }.toSet

def strips(A: String => Boolean, B: Set[String]): Set[String] =
  B.flatMap(s => strip(A, s))

def checkP(ms: Set[String], r: Rexp): Boolean = {
  val impl = shifts(ms, r)
  val spec = strips(language(r), ms)
  //println(s"shifts=$impl , strips=$spec")
  impl == spec
}
@main
def test1() = {
  val r1 = CHAR('a')
  println(s"r1: ${checkP(Set("a","aa","b"), r1)}")
}

@main
def test2() = {
  val r2 = ONE
  println(s"r2: ${checkP(Set("","a","aa"), r2)}")
}

@main
def test3() = {
  val r3 = SEQ(CHAR('a'), CHAR('b'))
  println(s"r3: ${checkP(Set("ab","abc","x"), r3)}")
}

@main
def test4() = {
  val r4 = ALT(CHAR('a'), CHAR('b'))
  println(s"r4: ${checkP(Set("ax","bx","ab","ba"), r4)}")
}

@main
def test5() = {
  val r5 = STAR(CHAR('a'))
  println(s"r5: ${checkP(Set("a","aa","ba",""), r5)}")
}

@main
def test6() = {
  val rLeft  = ALT(CHAR('a'), SEQ(CHAR('a'), CHAR('b')))
  val rRight = ALT(CHAR('c'), SEQ(CHAR('b'), CHAR('c')))
  val r6     = SEQ(rLeft, rRight)
  println(s"r6: ${checkP(Set("abc","abbc","x"), r6)}")
}

@main
def test7() = {
  val r  = NTIMES( %("a") | "aa",3)
  println(s"r7: ${checkP(Set("aa","a"), r)}")
}


import scala.collection.parallel.CollectionConverters._

@main
def testAll() = {
  given rexp_cdata : CDATA[Rexp] = List(
        (0, _ => ONE),
        (0, _ => ZERO),
        (0, _ => CHAR('a')),
        (0, _ => CHAR('b')),
        (0, _ => CHAR('c')),
        (1, cs => STAR(cs(0))),
        (1, cs => NTIMES(cs(0),new scala.util.Random().nextInt(30) + 1 )),
        (2, cs => ALT(cs(0), cs(1))),
        (2, cs => SEQ(cs(0), cs(1)))
      )
  val alphabet = LazyList('a', 'b')
  
  val numRegexes = BigInt(10_000_000_000L)
  val batchSize = BigInt(100_000L) 
  
  val batches = (BigInt(0) to numRegexes by batchSize).toVector.par
  batches.foreach { start =>
    val end = (start + batchSize - 1).min(numRegexes)
    for (i <- start to end) {
      val r = enumerate.decode(i)
      if (i % 100_000 == 0) { print("*") }
      for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "") {
         
         val checkResult=checkP(Set(s), r)

          if (! checkResult) {
            println(s"[${i}]-\n reg: $r \nstr: $s")
            println(s"\n${pp(r)}")
            val impl = shifts(Set(s), r)
            val spec = strips(language(r), Set(s))
            println(s"shifts= $impl\nstrips=$spec")
            print("Type 'N' to exit, anything else to continue: ")
            val input = scala.io.StdIn.readLine()
            if (input.trim.toLowerCase == "n") {
                System.exit(1)
            }//end of if n -> exit
        }//end of if markedMatcher != derMatcher
      }// end of for s <- regenerate.generate_up_to(alphabet)(10)(r)
    }//end of for i <- start to end
  }// end of batches.foreach
  println("\nAll tests passed!")
}// end of strongTestNoSTARParallel
