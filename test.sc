
import $file.enumerate, enumerate.{decode as enumDecode, CDATA}
import $file.regenerate, regenerate._
import $file.rexp, rexp._
import rexp.Rexp.*
import $file.derivativesBitcode, derivativesBitcode._




val alphabet: LazyList[Char] = LazyList('a', 'b', 'c')

// Bring in the CDATA for Rexp construction
given rexp_cdata: CDATA[Rexp] = List(

  (0, _ => ONE),
  (0, _ => CHAR('a')),
  (0, _ => CHAR('b')),
  (0, _ => CHAR('c')),
  (1, cs => STAR(cs(0))),
  (2, cs => ALT(cs(0), cs(1))),
  (2, cs => SEQ(cs(0), cs(1)))
)

//  
//  (0, _ => ZERO),
//  (1, cs => STAR(cs(0))),
//
//

val numRegexes = 100
val maxStringsPerRegex = 5

@main
def runCombinedTests(): Unit = {
  println(s"Testing first $numRegexes regexes with up to $maxStringsPerRegex matching strings each\n")

  for (i <- 0 to numRegexes) {
    val r = enumDecode(i)
    val strings = generate_up_to(alphabet)(10)(r).take(maxStringsPerRegex).toList

    println(s"\n[$i] Regex: $r")
    println(pp(r))
    println(s"strings=$strings")

    var allMatched = true
    for ((s, index) <- strings.zipWithIndex) {
      val sList = s.toList
      
      val bitcode1 = bitsToInts(lex(r, sList).getOrElse(Nil))
      val result1=matcher(r, sList)

      val finReg2 = bders(sList, internalize(r))
      val bitcode2 = bmkeps(finReg2)
      val result2 = bnullable(finReg2)

      println(s"Marked: input=$s , BitCode=$bitcode1, result=$result1")
      println(s"Derivatives: input=$s , BitCode=$bitcode2, result=$result2 \n")

      if (bitcode1.toList == bitcode2.toList) {
            println(s"Bitcodes match for input=$s")
            } else {
            println(s"Error: Bitcode mismatch for input=$s")
            }

      val bitMatch = bitcode1 == bitcode2
      val resultMatch = result1 == result2

      if (!bitMatch || !resultMatch) {
        allMatched = false
      }

    }

    if(allMatched)
    println("All strings matched")
    else
    println("Some strings did not match")
  }


}

def bitsToInts(bits: List[Bit]): List[Int] = bits.map {
  case Z => 0
  case S => 1
}