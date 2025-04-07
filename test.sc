import $file.enumerate, enumerate.{decode as enumDecode, CDATA}
import $file.regenerate, regenerate._
import $file.rexp, rexp._
import rexp.Rexp.*
import $file.derivativesBitcode, derivativesBitcode._

val alphabet: LazyList[Char] = LazyList('a', 'b', 'c')

given rexp_cdata: CDATA[Rexp] = List(
  (0, _ => ONE),
  (0, _ => CHAR('a')),
  (0, _ => CHAR('b')),
  (0, _ => CHAR('c')),
  //(1, cs => STAR(cs(0))),
  (2, cs => ALT(cs(0), cs(1))),
  (2, cs => SEQ(cs(0), cs(1)))
)

val numRegexes = 10L//100_000_000L
val maxStringsPerRegex = 5

@main
def test1List(): Unit = {
  println(s"🔍 Testing first $numRegexes regexes with up to $maxStringsPerRegex matching strings each\n")
  println("=" * 50)
  var allPassed = true
  var mismatchCount = 0
  var mismatches = List.empty[(Rexp, List[Char], List[Int], List[Int], Boolean, Boolean)]

  for (i <- 0L to numRegexes) {
    val regex = enumDecode(i)
    val testStrings = generate_up_to(alphabet)(10)(regex).take(maxStringsPerRegex).toList
    
    println(s"[$i]Regex: \n${pp(regex)}")
 

    for (str <- testStrings) {
      
      val sList = str.toList
      println(s"input=$str")
      val markBitcode = bitsToInts(lex(regex, sList).getOrElse(Nil))
      val markResult = matcher(regex, sList)

      val derivativeR = bders(sList, internalize(regex))
      val derivBitcode = bmkeps(derivativeR)
      val derivResult = bnullable(derivativeR)

      val bitMatch = markBitcode == derivBitcode
      val resultMatch = markResult == derivResult

      if (!bitMatch || !resultMatch) {
        allPassed = false
        mismatchCount += 1
        mismatches ::= (regex, sList, markBitcode, derivBitcode, markResult, derivResult)
      }

      // Optional per-string output:
      println(s"Bitcode1 = $markBitcode | Bitcode2 = $derivBitcode")
      println(s"${if (bitMatch && resultMatch) "✓" else "✗"}") 
    }
    println("=" * 50)
  }

  if (allPassed) {
    println("\nAll strings and bitcodes matched")
  } else {
    println(s"\nFound $mismatchCount mismatches: \n")

    for ((regex, input, bc1, bc2, res1, res2) <- mismatches.reverse) {
      println(s"Regex: $regex")
      println(s"Input: ${input.mkString}")
      println(s"Marked Bitcode: $bc1")
      //println(s"Deriv  Bitcode:result $bc2")
      println(s"Marked Result: $res1, Deriv Result: $res2")
      println("-" * 40)
    }

    val nMatches = numRegexes * maxStringsPerRegex - mismatchCount
    println(s"\nSummary: $mismatchCount mismatches and $nMatches matches = ${numRegexes * maxStringsPerRegex}")
  }
}

@main
def test2Print(): Unit = {
  println(s"🔍 Testing first $numRegexes regexes with up to $maxStringsPerRegex matching strings each\n")
  println("=" * 50)
  var allPassed = true
  var mismatchCount = 0

  for (i <- 0L to numRegexes) {
    val regex = enumDecode(i)
    for (str <- regenerate.generate_up_to(alphabet)(10)(regex).take(maxStringsPerRegex) if str != "" ) {
      val sList = str.toList
      val markBitcode = bitsToInts(lex(regex, sList).getOrElse(Nil))
      val derivativeR = bders(sList, internalize(regex))
      val derivBitcode = bmkeps(derivativeR)
      val bitMatch = markBitcode == derivBitcode
      if (!bitMatch) {
        allPassed = false
        mismatchCount += 1
        println(s"Bitcode1 = $markBitcode\nBitcode2 = $derivBitcode/n ${pp(regex)}")
        println("=" * 50)
      }
    }
    
  }

  if (allPassed) 
    println("\nAll strings and bitcodes matched")
   else 
    println(s"\nFound $mismatchCount mismatches: \n")
}

def bitsToInts(bits: List[Bit]): List[Int] = bits.map {
  case Z => 0
  case S => 1
}

/* @main
def test3() = {
  given rexp_cdata : CDATA[Rexp] = List(
        (0, _ => ONE),
        (0, _ => ZERO),
        (0, _ => CHAR('a')),
        (0, _ => CHAR('b')),
        (0, _ => CHAR('c')),
        //(1, cs => STAR(cs(0))),
        (2, cs => ALT(cs(0), cs(1))),
        (2, cs => SEQ(cs(0), cs(1)))
      )

  val alphabet = LazyList('a', 'b')
  for (i <- (0L to 100_000_000L)) {
    val r = enumerate.decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "")
      { val v1 = lex(r, s.toList)
        val v2 = rebit.lex(r, s.toList)
        if (v1.isDefined && v1.get != v2) {
          println(s"reg: $r str: $s")
          println(s"mark: ${v1.get} bder: $v2")
        }
      }
  }
} */