import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.rebit

extension (xs: List[Bits]) {
  def <*> (ys: List[Bits]): List[Bits] =
    for (x <- xs; y <- ys) yield x ::: y
  def <+> (y: Bit): List[Bits] =
    for (x <- xs) yield x :+ y
  def <++>(ys: Bits): List[Bits] =
    xs.map(_ ++ ys)
  def <::>(y: Bit): List[Bits] =
    for (x <- xs) yield y :: x
}

type Marks = Set[Mark]

extension (ms: Marks) {
  def <:+>(b: Bit): Marks =
    ms.map(m => m.copy(bits = m.bits :+ b))
  def <::+>(bs: Bits): Marks =
    ms.map(m => m.copy(bits = m.bits ::: bs))
}
extension (m: Mark) {
  def <:+>(b: Bit): Mark =
    m.copy(bits = m.bits :+ b)
}

def mkeps2(r: Rexp) : Bits = r match {
  case ONE => Nil
  case ALT(r1, r2) => if (nullable(r1)) Lf :: mkeps2(r1) else Ri :: mkeps2(r2)
  case SEQ(r1, r2) => mkeps2(r1) ++ mkeps2(r2)
  case STAR(_)     => List(En)
  case NTIMES(_,_) => List(EnT)
  case AND(r1,r2)  => mkeps2(r1) ++ mkeps2(r2)
}

def shifts(ms: Marks, r: Rexp): Marks = (r: @unchecked) match {
  case ZERO => Set()
  case ONE  => Set()
  case CHAR(d) =>
    for (m <- ms if m.str != Nil && m.str.head == d) yield m.copy(str = m.str.tail)
  case ALT(r1, r2) =>
    //shifts(ms <:+> Lf, r1) ++ shifts(ms <:+> Ri, r2)
    val p1=shifts(ms <:+>Lf,r1)
    if(p1.exists(_.str == Nil)) p1
    else
        p1 ++ shifts(ms<:+> Ri,r2)

  case SEQ(r1, r2) =>
  val ms1 = shifts(ms, r1)
  (nullable(r1), nullable(r2)) match {
    case (true, true) =>
      val p1 = ms1 <::+> mkeps2(r2)
      if (p1.exists(_.str == Nil)) p1
      else {
        val p2 = shifts(ms1,r2) //ms1.flatMap(m => shifts(Set(m), r2))
        if (p2.exists(_.str == Nil)) p1 ++ p2
        else {
          val p3 = shifts(ms <::+> mkeps2(r1), r2)
          if (p3.exists(_.str == Nil)) p1++p2++p3
          else p1 ++ p2 ++ p3
        }
      }

    case (true, false) =>
      val p2 = ms1.flatMap(m => shifts(Set(m), r2))
      if (p2.exists(_.str == Nil)) p2
      else {
        val p3 = shifts(ms <::+> mkeps2(r1), r2)
        if (p3.exists(_.str == Nil)) p3 else p2 ++ p3
      }

    case (false, true) =>
      val p1 = ms1 <::+> mkeps2(r2)
      if (p1.exists(_.str == Nil)) p1
      else {
        val p2 =shifts(ms1,r2) // ms1.flatMap(m => shifts(Set(m), r2))
        if (p2.exists(_.str == Nil)) p2 else p1 ++ p2
      }

    case (false, false) =>
      shifts(ms1,r2)  
      //ms1.flatMap(m => shifts(Set(m), r2))
  }
  
  
  case SEQ(r1, r2) =>
    val ms1 = shifts(ms, r1)
    (nullable(r1), nullable(r2)) match {
      case (true, true) =>
        (ms1 <::+> mkeps2(r2)) ++ ms1.flatMap(m => shifts(Set(m), r2)) ++ shifts(ms <::+> mkeps2(r1), r2)
      case (true, false) =>
        ms1.flatMap(m => shifts(Set(m), r2)) ++ shifts(ms <::+> mkeps2(r1), r2)
      case (false, true) =>
        (ms1 <::+> mkeps2(r2)) ++ ms1.flatMap(m => shifts(Set(m), r2))
      case (false, false) =>
        ms1.flatMap(m => shifts(Set(m), r2))
    }
  case STAR(r) =>
    val ms1 = shifts(ms <:+> Nx, r)
    if (ms1.isEmpty) Set() else (ms1 <:+> En) ++ ms1.flatMap(m => shifts(Set(m), STAR(r)))

  case NTIMES(r0, n) if n == 0 =>
    ms <:+> EnT
  case NTIMES(r0, n) =>
    if ((ms.exists(_.str == Nil) && n != 0) && !nullable(r0)) Set()
    else {
      val ms1 = shifts(ms <:+> NxT, r0)
      if (nullable(r0)) (ms1 <:+> EnT) ++ ms1.flatMap(m => shifts(Set(m), NTIMES(r0, n - 1)))
      else ms1.flatMap(m => shifts(Set(m), NTIMES(r0, n - 1)))
    }
  case AND(r1, r2) =>
    shifts(ms, r1) intersect shifts(ms, r2)
}

def mat(r: Rexp, s: String) : Marks ={
    val im = Mark(bits = List(), str = s.toList)
    shifts(Set(im), r)
  }

def matcher(r: Rexp, s: String) : Boolean =
  if s == "" then nullable(r)
  else {
    val im = Mark(bits = List(), str = s.toList)
    shifts(Set(im), r).exists(_.str == Nil)
  }

def lex(r: Rexp, s: String): Option[Bits] =
  if matcher(r, s) then
    if s == "" then Some(mkeps2(r))
    else shifts(Set(Mark(bits = List(), str = s.toList)), r).find(_.str == Nil).map(_.bits)
  else None

def lexer(r: Rexp, s: String) : Option[Val] =
  lex(r, s).map(dec2(r, _))




// 
@main 
def test1() = {
  println("=====Test====")
  val br2= ("a" | "ab") ~ ("c" | "bc")
  val s = "abc"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")
  println(mat(br2,s))
  println(lex(br2, s))
  commonTestCode(br2, s)
}

def commonTestCode( br2 : Rexp, s : String ) = {
  val markedBits=lex(br2, s)
  //val markedVal=lexer(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")
  //println(s"marked Val: ${markedVal}")
  println("------------------------")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}

@main
def testExamples(): Unit = {
  val tests = List(
    ("test1", %( "a" | "aa" ), "aa"),
    ("test2", %("a"), "aaa"),
    ("test3", %("aa") ~ %(%("a")), "aaaaaa"),
    ("test4", (%("a") ~ %("a")) ~ "a", "aaa"),
    ("test5", %(%(%("a"))), "aa"),
    ("test6", %("a" ~ %("a")), "aaa"),
    ("test7", %(%("a") ~ "a"), "aaa"),
    ("test8", %("a") ~ (%("a") ~ "a"), "aaa"),
    ("test9", %("a") ~ %("a"), "a" * 10),
    ("test10", %((ONE | "a") | "aa"), "aaa"),
    ("test11", (ONE | "a") ~ ("a" | "aa"), "aaa"),
    ("test12", ("a" | "ab") ~ ("c" | "bc"), "abc"),
    ("test13", (("a" | "b") | "ab") ~ ("bc" | "c" | "b"), "abc"),
    ("test14", (ONE | (ONE | "bc")) | (("a" | ONE) ~ ("a" | "aa")), "aa"),
    ("test15", (("a" | "b") ~ (ONE | "a")) ~ "a", "aa"),
    ("test16", ((ONE | "c") | %(ONE)) ~ "b", "b"),
    ("test17", %("a" ~ ONE), "aa"),
    ("test18", %("a" | %("b")), "ba"),
    ("test19", %(%("a") | "ab"), "aabaab"),
    ("test20", %("bb" | "b"), "bbb"),
    ("test21", ("a" ~ %("b")) ~ (("b" | ONE) ~ "a"), "aba"),
    ("test22", ("c" | ("a" | ONE)) ~ ("ab" | "b"), "ab"),
    ("test23", NTIMES("a", 3), "a" * 3),
    ("test24", %( "a") ~  %("a"), "a" * 4)
  )

  var passed = 0
  var failed = 0

  tests.zipWithIndex.foreach {
    case ((label, r, s), i) =>
      val marked = lex(r, s).getOrElse(Nil)
      val deriv  = rebit.lex(r, s.toList)

      if (marked != deriv) {
        failed += 1
        
        println(s"Test ${i + 1} FAILED: $label")
        println(s"Regex:\n${pp(r)}")
        println(s"Input string: $s")
        println(s"Marked bits    : $marked")
        println(s"Derivative bits: $deriv")
        println("--------------------------------------------------")
      } else {
        passed += 1
        println(s"Test ${i + 1} passed: $label")
      }
  }

  println(s"\n==== Test Summary ====")
  println(s"Passed: $passed")
  println(s"Failed: $failed")
}

// decoding of a value from a bitsequence
def decode_aux(r: Rexp, bs: Bits) : (Val, Bits) = ((r, bs): @unchecked) match {
  case (ONE, bs) => (Empty, bs)
  case (CHAR(c), bs) => (Chr(c), bs)
  case (ALT(r1, r2), Lf::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    (Left(v), bs1)
  }
  case (ALT(r1, r2), Ri::bs) => {
    val (v, bs1) = decode_aux(r2, bs)
    (Right(v), bs1)
  }
  case (SEQ(r1, r2), bs) => {
    val (v1, bs1) = decode_aux(r1, bs)
    val (v2, bs2) = decode_aux(r2, bs1)
    (Sequ(v1, v2), bs2)
  }
  case (STAR(_), En::bs) => (Stars(Nil), bs)

  case (STAR(r1), Nx::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    val (Stars(vs), bs2) = (decode_aux(STAR(r1), bs1)  : @unchecked)
    (Stars(v::vs), bs2)
  }
  
  case (NTIMES(r1,n), NxT::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    val (Nt(vs,ns), bs2) = (decode_aux(NTIMES(r1,n-1), bs1)  : @unchecked)
    (Nt(v::vs,n), bs2)
  }
  case (NTIMES(_,_), EnT::bs) => (Nt(Nil,0), bs)
}

def dec2(r: Rexp, bs: Bits) = decode_aux(r, bs) match {
  case (v, Nil) => v
  case _ => throw new Exception("Not decodable")
}

// pretty-printing Rexps
def implode(ss: Seq[String]) = ss.mkString("\n")
def explode(s: String) = s.split("\n").toList

def lst(s: String) : String = explode(s) match {
  case hd :: tl => implode(" └" ++ hd :: tl.map("  " ++ _))
  case Nil => ""
}

def mid(s: String) : String = explode(s) match {
  case hd :: tl => implode(" ├" ++ hd :: tl.map(" │" ++ _))
  case Nil => ""
}

def indent(ss: Seq[String]) : String = ss match {
  case init :+ last => implode(init.map(mid) :+ lst(last))
  case _ => "" 
}

def pp(e: Rexp) : String = (e: @unchecked) match { 
  case ZERO => "0\n"
  case ONE => s"1 \n"
  case CHAR(c) => s"$c\n"
  case ALT(r1, r2) => "ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => "SEQ\n" ++ pps(r1, r2)
  case STAR(r) => s"STAR\n" ++ pps(r)
  case NTIMES(r, n) => s"NTIMES($n)\n" ++ pps(r)
  case AND(r1, r2) => "AND\n" ++ pps(r1, r2)
}

def pps(es: Rexp*) = indent(es.map(pp))

// extracts a string from a value
def flatten(v: Val) : String = v match {
   case Empty => ""
   case Chr(c) => c.toString
   case Left(v) => flatten(v)
   case Right(v) => flatten(v)
   case Sequ(v1, v2) => flatten(v1) ++ flatten(v2)
   case Stars(vs) => vs.map(flatten).mkString
   case Nt(vs, _) => vs.map(flatten).mkString
   //case Rec(_, v) => flatten(v)
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
          val markedBits=lex(r, s).getOrElse(Nil)
          val derBits= rebit.lex(r, s.toList)
          if (markedBits != derBits) {
            println(s"[${i}]-\n reg: $r \nstr: $s")
            println(s"\n${pp(r)}")
            println(s"markedBits= $markedBits\nderBits=$derBits")
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


