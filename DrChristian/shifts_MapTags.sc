import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.rexps, rexps._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.rebit
import scala.collection.mutable

type Marks = Set[Int]

val trace = mutable.HashMap.empty[(Int, Int), mutable.HashMap[Int, Int]]

def record(id: Int, m: Int, start: Int, choice: Int): Unit = 
    trace.getOrElseUpdate((id, m), mutable.HashMap.empty)(start) = choice

def shifts(ms: Marks, s: String, r: RexpS): Marks = r match {
  case ZEROS => Set.empty
  case ONES  => Set.empty
  
  case CHARS(c) => 
    ms.flatMap { m => 
      if (m < s.length && s(m) == c) Set(m + 1) 
      else Set.empty
    }
  
  case ALTS(r1, r2, id) =>
    ms.flatMap { start =>
      val left  = shifts(Set(start), s, r1)
      val right = shifts(Set(start), s, r2)
      left.foreach(m => record(id, m, start, 0))
      right.foreach(m => 
        if (!left.contains(m)) record(id, m, start, 1)
      )
      left ++ right
    }
  case SEQS(r1, r2, id) =>
    val n1 = nullableS(r1)
    val n2 = nullableS(r2)
    ms.flatMap { start =>
      val ms1 = shifts(Set(start), s, r1)
      
      // Case 1: r1 consumes, r2 skipped 
      val skipR2 = if (n2) {
        ms1.foreach(m => record(id, m, start, m))
        ms1
      } else Set.empty
      
      // Case 2: Both r1 and r2 consume
      val both = ms1.flatMap { split =>
        val ms2 = shifts(Set(split), s, r2)
        ms2.foreach(m => record(id, m, start, split))
        ms2
      }
      
      // Case 3: r1 skipped, r2 consumes 
      val skipR1 = if (n1) {
        val ms2 = shifts(Set(start), s, r2)
        ms2.foreach(m => record(id, m, start, start))
        ms2
      } else Set.empty
      
      skipR2 ++ both ++ skipR1
    }
  
  case STARSS(r1, id) =>
    val ms1 = ms.flatMap { start =>
      val out = shifts(Set(start), s, r1)
      out.foreach(m => if (m > start) record(id, m, start, start))
      out
    }
    
    if (ms1.isEmpty) Set.empty
    else ms1 ++ shifts(ms1, s, STARSS(r1, id))

  case NTIMESS(r1, n) =>
    if (n == 0) Set.empty
    else if (n == 1) shifts(ms, s, r1)
    else {
      val ms1 = shifts(ms, s, r1)
      if (ms1.isEmpty) Set.empty
      else {
        val rest = shifts(ms1, s, NTIMESS(r1, n - 1))
        if (nullableS(r1)) ms1 ++ rest else rest
      }
    }

  case ANDS(r1, r2) =>
    shifts(ms, s, r1).intersect(shifts(ms, s, r2))
}

def back(r: RexpS, s: String, a: Int, b: Int): Val = r match {
  case ONES =>
    if (a == b) Empty else Invalid

  case CHARS(c) =>
    if (b == a + 1 && a >= 0 && b <= s.length && s(a) == c) Chr(c) 
    else Invalid

  case ALTS(r1, r2, id) =>
    trace.get((id, b)).flatMap(_.get(a)) match {
      case Some(0) => 
        back(r1, s, a, b) match {
          case Invalid => Invalid
          case v => Left(v)
        }
      case Some(1) => 
        back(r2, s, a, b) match {
          case Invalid => Invalid
          case v => Right(v)
        }
      case _ => Invalid
    }

  case SEQS(r1, r2, id) =>
    trace.get((id, b)).flatMap(_.get(a)) match {
      case Some(split) if split == b =>
        back(r1, s, a, b) match {
          case Invalid => Invalid
          case v1 => Sequ(v1, rexps.mkeps(r2))
        }
      
      case Some(split) if split == a =>
        back(r2, s, a, b) match {
          case Invalid => Invalid
          case v2 => Sequ(rexps.mkeps(r1), v2)
        }
      
      case Some(split) =>
        (back(r1, s, a, split), back(r2, s, split, b)) match {
          case (Invalid, _) => Invalid
          case (_, Invalid) => Invalid
          case (v1, v2) => Sequ(v1, v2)
        }
      
      case None => Invalid
    }
  
  case STARSS(r1, id) =>
    if (a == b) Stars(Nil)
    else {
      trace.get((id, b)).map(_.keys.filter(p => p >= a && p < b).toList.sorted) match {
        case Some(prev :: _) =>
          (back(r1, s, prev, b), back(STARSS(r1, id), s, a, prev)) match {
            case (Invalid, _) => Invalid
            case (_, Invalid) => Invalid
            case (v1, Stars(vs)) => Stars(v1 :: vs)
            case _ => Invalid
          }
        case _ => Invalid
      }
    }

  case _ => Invalid
}



def mat(r: Rexp, s: String): Marks = {
  val rs = intern(r)
  shifts(Set(0), s, rs)
}

def matcher(r: Rexp, s: String): Boolean =
  if (s == "") nullable(r) else mat(r, s).contains(s.length)

def isInvalid(v: Val): Boolean = v match {
  case Invalid => true
  case _       => false
}

def lexer(r: Rexp, s: String): Val = {
  trace.clear()
  val rs = intern(r)
  val ms = shifts(Set(0), s, rs)
  val accepted = ms.contains(s.length)

  println(s"RexpS:\n${ppId(rs)}")
  println(s"s=$s")
  println(s"accepted=$accepted")
  println(s"marks=$ms")
  
  println("(id,m) : from -> choice")
  trace.toList.sortBy { case ((id, m), _) => (id, m) }
  .foreach { case ((id, m), mp) =>
        val pairs = mp.toList.map { case (start, choice) => s"$start -> $choice" }.mkString(", ")
        println(s"($id,$m): $pairs")
        }

  if (!accepted) Invalid
  else {
    val v = back(rs, s, 0, s.length)
    if (!isInvalid(v)) v else Invalid
  }
}






@main
def tests() = {
  val reg = ("aa") | ("a" ~ (ONE ~ "a"))
  val s   = "aa"
  //println(s"1- $s: r=\n${pp(reg)}  ${mat(reg, s)} ")
  println(s"Derivative Value=${rebit.blexer(reg, s)}")
  println(lexer(reg, s))
  println("-"*40)

  val reg2 = %("a"|"aa")
  val s2   = "aaa"
  //println(s"2- $s2: r=\n${pp(reg2)}  ${mat(reg2, s2)} ")
  println(s"Derivative Value=${rebit.blexer(reg2, s2)}")
  println(lexer(reg2, s2))
  println("-"*40)

  val reg3 = (ONE | %("a"))
  val s3   = "a"
  //println(s"3- $s3: r=\n${pp(reg3)}  ${mat(reg3, s3)} ")
  println(s"Derivative Value=${rebit.blexer(reg3, s3)}")
  println(lexer(reg3, s3))
  println("-"*40)

  val reg4 = ("a"| %("a") )
  val s4   = "a"
  //println(s"4- $s4: r=\n${pp(reg4)}  ${mat(reg4, s4)} ")
  println(s"Derivative Value=${rebit.blexer(reg4, s4)}")
  println(lexer(reg4, s4))
  println("-"*40)

  val reg5 = ( %(ONE) ~ ("a") )
  val s5   = "a"
  //println(s"5- $s5: r=\n${pp(reg5)}  ${mat(reg5, s5)} ")
  println(s"Derivative Value=${rebit.blexer(reg5, s5)}")
  println(lexer(reg5, s5))
  println("-"*40)

  val reg6 = ( %("a") | %("aa") )
  val s6   = "aa"
  //println(s"6- $s6: r=\n${pp(reg6)}  ${mat(reg6, s6)} ")
  println(s"Derivative Value=${rebit.blexer(reg6, s6)}")
  println(lexer(reg6, s6))
  println("-"*40)

  val reg7 = ( ( ONE | "a" ) ~ %("a") )
  val s7   = "a"
  //println(s"7- $s7: r=\n${pp(reg7)}  ${mat(reg7, s7)} ")
  println(s"Derivative Value=${rebit.blexer(reg7, s7)}")
  println(lexer(reg7, s7))
  println("-"*40)

  val reg8 = ( ONE ~ "a" ) | ("a" ~ ONE )
  val s8   = "a"
  //println(s"8- $s8: r=\n${pp(reg8)}  ${mat(reg8, s8)} ")
  println(s"Derivative Value=${rebit.blexer(reg8, s8)}")
  println(lexer(reg8, s8))
  println("-"*40)

  val reg9 = (( "a" ~ ONE ) | ( ONE ~ "a" )) ~ %("a")
  val s9   = "a"
  //println(s"9- $s9: r=\n${pp(reg9)}  ${mat(reg9, s9)} ")
  println(s"Derivative Value=${rebit.blexer(reg9, s9)}")
  println(lexer(reg9, s9))
  println("-"*40)

  val reg10 = ( ("a" | "b") | "b") 
  val s10   = "b"
  //println(s"10- $s10: r=\n${pp(reg10)}  ${mat(reg10, s10)} ")
  println(s"Derivative Value=${rebit.blexer(reg10, s10)}")
  println(lexer(reg10, s10))
  println("-"*40)
}

@main
def test1() = {
  val reg2 = %("a"|"aa")
  val s2   = "aaa"
  //println(s"2- $s2: r=\n${pp(reg2)}  ${mat(reg2, s2)} ")
  println(s"Derivative Value=${rebit.blexer(reg2, s2)}")
  println(lexer(reg2, s2))
  println("-"*40)
}

@main
def test2() = {
  val reg2 = %("a"|"aa")
  val s2   = "aaaaa"
  println(s"2- $s2: r=\n${pp(reg2)}  ${mat(reg2, s2)} ")
  println(s"Derivative Value=${rebit.blexer(reg2, s2)}")
  println(lexer(reg2, s2))
  println("-"*40)
}









def ppId(e: RexpS): String = (e: @unchecked) match {
  case ZEROS           => "0\n"
  case ONES            => "1 \n"
  case CHARS(c)        => s"$c\n"
  case ALTS(r1, r2, i) => s"ALT:$i\n" ++ ppsId(r1, r2)
  case SEQS(r1, r2, i) => s"SEQ:$i\n" ++ ppsId(r1, r2)
  case STARSS(r, i)    => s"STAR:$i\n" ++ ppsId(r)
  case NTIMESS(r, n)   => s"NTIMES($n)\n" ++ ppsId(r)
  case ANDS(r1, r2)    => "AND\n" ++ ppsId(r1, r2)
}

def ppsId(es: RexpS*) = rexps.indent(es.map(ppId))