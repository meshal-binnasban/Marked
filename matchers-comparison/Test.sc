

import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.Derivatives
import $file.Play
import $file.Shifts

import scala.language.implicitConversions


def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

@main
def test1() = {
  println("=====Test====")
  val r = ("a" | "ab") ~ ("c" | "bc")
  val s = "abc"
  println("=string=")
  println(s)

  println(s"Derivatives: ${Derivatives.matcher(r, s)}")
  println(s"Play: ${Play.matcher(r, s)}")
  println(s"Shifts: ${Shifts.matcher(r, s)}")
  val derivatives=time_needed(1000,Derivatives.matcher(r, s))
  val play=time_needed(1000,Play.matcher(r, s))
  val shifts=time_needed(1000,Shifts.matcher(r, s))

  val list = List( ("Derivatives", derivatives), ("Play",play),("Shifts",shifts))
  println(s"=Times=")
  println(s"Derivatives= $derivatives")
  println(s"Play= $play")
  println(s"Shifts= $shifts")

  println(list.sortBy(_._2))
  
}

@main
def testAll() = {
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

  var totalDerivatives = 0.0
  var totalPlay        = 0.0
  var totalShifts      = 0.0
  var regexCount       = 0L

  for (i <- (0L to 100_000L)) {//100_000_000L
    val r = enumerate.decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "")
      { 
        val shifts=time_needed(1000,Shifts.matcher(r, s))
        val play=time_needed(1000,Play.matcher(r, s))
        val derivatives=time_needed(1000,Derivatives.matcher(r, s))
        
        totalDerivatives += derivatives
        totalPlay += play
        totalShifts += shifts
        regexCount += 1
      } 
  }
  println("\n\n===== Averages =====")
  println("Derivatives = " + (totalDerivatives / regexCount))
  println("Play        = " + (totalPlay / regexCount))
  println("Shifts      = " + (totalShifts / regexCount))
  val list=List( ("Derivatives", totalDerivatives / regexCount), ("Play",totalPlay / regexCount), ("Shifts",totalShifts / regexCount))
  list.sortBy(_._2).foreach { case (name, avg) => println(s"$name = $avg")}
}

import $ivy.`org.scala-lang.modules::scala-parallel-collections:1.0.4`
import scala.collection.parallel.CollectionConverters._

@main
def testAllP() = {
  given rexp_cdata : CDATA[Rexp] = List(
        (0, _ => ONE),
        (0, _ => ZERO),
        (0, _ => CHAR('a')),
        (0, _ => CHAR('b')),
        (0, _ => CHAR('c')),
        (1, cs => STAR(cs(0))),
        //(1, cs => NTIMES(cs(0),new scala.util.Random().nextInt(30) + 1 )),
        (2, cs => ALT(cs(0), cs(1))),
        (2, cs => SEQ(cs(0), cs(1)))
      )
  val alphabet = LazyList('a', 'b')
  var totalDerivatives = 0.0
  var totalPlay        = 0.0
  var totalShifts      = 0.0
  var regexCount       = 0L

  val numRegexes = BigInt(10_000_000L)
  val batchSize = BigInt(100_000L) 
  
  val batches = (BigInt(0) to numRegexes by batchSize).toVector.par
  batches.foreach { start =>
    val end = (start + batchSize - 1).min(numRegexes)
    for (i <- start to end) {
      val r = enumerate.decode(i)
      if (i % 100_000 == 0) { print("*") }
      for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "") {

        val shifts=time_needed(1000,Shifts.matcher(r, s))
        val play=time_needed(1000,Play.matcher(r, s))
        val derivatives=time_needed(1000,Derivatives.matcher(r, s))
        
        totalDerivatives += derivatives
        totalPlay += play
        totalShifts += shifts
        regexCount += 1

      }// end of for s <- regenerate.generate_up_to(alphabet)(10)(r)
    }//end of for i <- start to end
  }// end of batches.foreach
  println("\n\n===== Averages =====")
  println("Derivatives = " + (totalDerivatives / regexCount))
  println("Play        = " + (totalPlay / regexCount))
  println("Shifts      = " + (totalShifts / regexCount))
  val list=List( ("Derivatives", totalDerivatives / regexCount), ("Play",totalPlay / regexCount), ("Shifts",totalShifts / regexCount))
  list.sortBy(_._2).foreach { case (name, avg) => println(s"$name = $avg")}
}// end of testAllP
