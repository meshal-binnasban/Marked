import $file.enumerate, enumerate.{decode as enumDecode, CDATA} , $file.regenerate, regenerate._
import $file.rexp, rexp._, rexp.Rexp._, rexp.VALUE._
import $file.play_explicit_bits_2, play_explicit_bits_2._
import $file.derivativesBitcode, derivativesBitcode._


val alphabet: LazyList[Char] = LazyList('a', 'b', 'c')

given rexp_cdata: CDATA[Rexp] = List(
 // (0, _ => ONE),
  (0, _ => CHAR('a')),
  (0, _ => CHAR('b')),
  (0, _ => CHAR('c')),
  (1, cs => STAR(cs(0))),
  (2, cs => ALT(cs(0), cs(1))),
  (2, cs => SEQ(cs(0), cs(1)))
)

val numRegexes = 100_000L//100_000_000L
val maxStringsPerRegex = 5

@main
def test1(): Unit = {
  println(s"🔍 Testing first $numRegexes regexes with up to $maxStringsPerRegex matching strings each\n")
  println("=" * 50)
  var allPassed = true
  var mismatchCount = 0

  for (i <- 0L to numRegexes) {
    val regex = enumDecode(i)
    for (str <- regenerate.generate_up_to(alphabet)(10)(regex).take(maxStringsPerRegex) if (str != " " | str!="") ) {
      val sList = str.toList
      val markBitcode = lex(regex, sList).getOrElse(Nil)
      val convertedBitcode=convertMtoDBit2(markBitcode)
      println(s"regex = $regex, sList = $sList, convertedBitcode = $convertedBitcode , markBitcode= $markBitcode")
      val markValue=mDecode(markBitcode, regex)._1

      val derivativeR = bders(sList, internalize(regex))
      val derivBitcode = bmkeps(derivativeR)
      val derivValue=decode(derivBitcode, regex)._1

      val valueMatch = compareResults(markValue, derivValue)
      val bitMatch = bitsToInts(convertedBitcode) == derivBitcode

      if (!valueMatch || !bitMatch) {
        allPassed = false
        mismatchCount += 1
        if(mismatchCount > 1000){
        println(s"convertedBitcode =$convertedBitcode , bitMatch = $derivBitcode")

        println(s"convertedBitcode type = ${convertedBitcode.getClass}")
        println(s"bitMatch type = ${derivBitcode.getClass}")
        println(s"Mark Value = $markValue\nDeriv Value = $derivValue\n ${rexp.pp(regex)}")
        println(s"Mark Bits = $convertedBitcode\nDeriv Bits = $derivBitcode\n")
        println(s"Input: ${sList}")
        println("=" * 50)
        }

        if(mismatchCount > 1200){
          return
        }

      }
    }
    
  }// end of for

  if (allPassed) 
    println("\nAll strings and bitcodes matched")
   else 
    println(s"\nFound $mismatchCount mismatches: \n")
}

@main
def test2(): Unit = {
  println(s"🔍 Testing first $numRegexes regexes with up to $maxStringsPerRegex matching strings each\n")
  println("=" * 50)
  var allPassed = true
  var mismatchCount = 0

  for (i <- 0L to numRegexes) {
    val regex = enumDecode(i)
    for (str <- regenerate.generate_up_to(alphabet)(10)(regex).take(maxStringsPerRegex) if str != "" ) {
      val sList = str.toList
      val markBitcode = lex(regex, sList).getOrElse(Nil)
      val markValue=mDecode(markBitcode, regex)._1

      val derivativeR = bders(sList, internalize(regex))
      val derivBitcode = bmkeps(derivativeR)
      val derivValue=decode(derivBitcode, regex)._1

      val valueMatch = markValue == derivValue

      if (!valueMatch) {
        allPassed = false
        mismatchCount += 1

        if(mismatchCount > 10){
        println(s"Mark Value = $markValue\nDeriv Value = $derivValue\n ${rexp.pp(regex)}")
        println(s"Mark Bits = $markBitcode\nDeriv Bits = $derivBitcode\n")
        println(s"Input: ${sList}")
        println("=" * 50)
        }

        if(mismatchCount > 2000){
        println(s"Mark Value = $markValue\nDerivBitcode = $derivValue\n ${rexp.pp(regex)}")
        println(s"Input: ${sList}")
        println("=" * 50)
          //return
        }

      }
    }
    
  }

  if (allPassed) 
    println("\nAll strings and bitcodes matched")
   else 
    println(s"\nFound $mismatchCount mismatches: \n")
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

/*
@main
def test1(): Unit = {
  / println(s"🔍 Testing first $numRegexes regexes with up to $maxStringsPerRegex matching strings each\n")
  println("=" * 50)
  var allPassed = true
  var mismatchCount = 0
  var mismatches = List.empty[(Rexp, List[Char], List[Int], List[Int])]

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
        mismatches = (regex, sList, markBitcode, derivBitcode) :: mismatches      }
   }
  }

  if (allPassed) {
    println("\nAll strings and bitcodes matched")
  } else {
    println(s"\nFound $mismatchCount mismatches: \n")

    for ((regex, input, bc1, bc2) <- mismatches.reverse) {
      println(s"Regex: $regex")
      println(s"Input: ${input}")
      println(s"Marked Bitcode: $bc1")
      println(s"Deriv  Bitcode: $bc2")
      println("-" * 40)
    }

    val nMatches = numRegexes * maxStringsPerRegex - mismatchCount
    println(s"\nSummary: $mismatchCount mismatches and $nMatches matches = ${numRegexes * maxStringsPerRegex}")
  } 
  }*/

