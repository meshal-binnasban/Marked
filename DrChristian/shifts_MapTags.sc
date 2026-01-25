import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.rexps, rexps._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.rebit
import scala.collection.mutable

type Marks = Set[Int]

// trace: (id, end) ----> Map[start --> choice/split]
val trace = mutable.HashMap.empty[(Int, Int), mutable.HashMap[Int, Int]]

def record(id: Int, end: Int, start: Int, choice: Int): Unit = 
    trace.getOrElseUpdate((id, end), mutable.HashMap.empty)(start) = choice

def shifts(ms: Marks, s: String, r: RexpS): Marks = r match {
  case ZEROS => Set.empty
  case ONES  => Set.empty
  
  case CHARS(c) => 
    ms.flatMap { m => 
      if (m < s.length && s(m) == c) Set(m + 1) 
      else Set.empty
    }
  
  case ALTS(r1, r2, id) =>
    ms.flatMap { m =>
      val out1 = shifts(Set(m), s, r1)
      val out2 = shifts(Set(m), s, r2)
      
      out1.foreach(end => record(id, end, m, 0))
      out2.foreach(end => record(id, end, m, 1))
      
      out1 ++ out2
    }
  
  case SEQS(r1, r2, id) =>
    val n1 = nullableS(r1)
    val n2 = nullableS(r2)
    
    ms.flatMap { m =>
      val ms1 = shifts(Set(m), s, r1)

      val r1All =
        if (n2) {
          ms1.foreach(end => record(id, end, m, end))
          ms1
        } else Set.empty[Int]
      
      val r1r2 = ms1.flatMap { k =>
        val ms2 = shifts(Set(k), s, r2)
        ms2.foreach(end => record(id, end, m, k))
        ms2
      }
      
      val r2All =
        if (n1) {
          val ms2 = shifts(Set(m), s, r2)
          ms2.foreach(end => record(id, end, m, m))
          ms2
        } else Set.empty[Int]

      r1All ++ r1r2 ++ r2All
    }
  
  case STARSS(r1, id) =>
    ms.flatMap { m =>
      val ms1 = shifts(Set(m), s, r1)
      ms1.foreach(end => if (end > m) record(id, end, m, m))
      
      if (ms1.isEmpty) Set.empty
      else ms1 ++ ms1.flatMap(k => shifts(Set(k), s, STARSS(r1, id)))
    }

  case NTIMESS(r1, n) =>
    if (n == 0) Set.empty
    else if (n == 1) shifts(ms, s, r1)
    else {
      ms.flatMap { m =>
        val out1 = shifts(Set(m), s, r1)
        val ks = out1
        if (ks.isEmpty) Set.empty
        else {
          val rest = ks.flatMap(k => shifts(Set(k), s, NTIMESS(r1, n - 1)))
          if (nullableS(r1)) out1 ++ rest else rest
        }
      }
    }

  case ANDS(r1, r2) =>
    val a = shifts(ms, s, r1)
    val b = shifts(ms, s, r2)
    a.intersect(b)
}

def back(r: RexpS, s: String, a: Int, b: Int): Val = r match {
  case ONES => if (a == b) Empty else Invalid

  case CHARS(c) =>
    if (b == a + 1 && a >= 0 && b <= s.length && s(a) == c) Chr(c) 
    else Invalid

  case ALTS(r1, r2, id) =>
    trace.get((id, b)) match {
      case Some(map) => map.get(a) match {
          case Some(0) =>  back(r1, s, a, b) match {
                            case Invalid => Invalid
                            case v => Left(v)
                           }
          case Some(1) =>  back(r2, s, a, b) match {
                            case Invalid => Invalid
                            case v => Right(v)
                           }
          case _ => Invalid
        }
      case None => Invalid
    }

  case SEQS(r1, r2, id) =>
    trace.get((id, b)) match {
        case None => Invalid
        case Some(map) => map.get(a) match {
                            case Some(k) =>
                            if (k == b) {
                            // r2 was skipped 
                            if (!nullableS(r2)) Invalid
                            else back(r1, s, a, b) match {
                                case Invalid => Invalid
                                case v1 => Sequ(v1, rexps.mkeps(r2))
                            }
                            } else if (k == a) {
                            // r1 was skipped 
                            if (!nullableS(r1)) Invalid
                            else back(r2, s, a, b) match {
                                case Invalid => Invalid
                                case v2 => Sequ(rexps.mkeps(r1), v2)
                            }
                            } else {
                            // Both consumed, split at k
                            back(r1, s, a, k) match {
                                case Invalid => Invalid
                                case v1 =>
                                back(r2, s, k, b) match {
                                    case Invalid => Invalid
                                    case v2 => Sequ(v1, v2)
                                }
                            }
                            }
          case None => Invalid
        }
    }
  
  case STARSS(r1, id) =>
    def loop(cur: Int): Val =
      if (cur == a) Stars(Nil)
      else {
        trace.get((id, cur)) match {
          case None => Invalid
          case Some(map) =>
            val prevs = map.keys.filter(p => p >= a && p < cur).toList.sorted
            prevs match {
              case Nil => Invalid
              case p :: _ =>
                back(r1, s, p, cur) match {
                  case Invalid => Invalid
                  case v1 =>
                    loop(p) match {
                      case Stars(vs) => Stars(v1 :: vs)
                      case _ => Invalid
                    }
                }
            }
        }
      }
    
    loop(b)

  case _ =>
    Invalid
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
  val out = shifts(Set(0), s, rs)
  val accepted = out.contains(s.length)

  println(s"RexpS:\n${ppId(rs)}")
  println(s"s=$s")
  println(s"accepted=$accepted")
  println(s"marks=$out")

  val rows = trace.toList
    .map { case ((id, m), xs) => (id, m, xs.toList.sorted.mkString("{", ", ", "}")) }
    .sortBy(x => (x._1, x._2))

  println("trace (id, end) -> ints:")
  rows.foreach { case (id, m, xs) => println(s"  ($id, $m) -> $xs") }

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