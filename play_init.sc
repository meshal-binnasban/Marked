//
// Algorithm from "A Play on Regular Expressions"
//
// the only difference is the use of BINIT which makes
// the match- and shift-function a bit more uniform
//
// Call with
//
//   amm play_init.sc test1
//   amm play_init.sc test2

// standard regexes
enum Rexp {
  case ZERO 
  case ONE 
  case CHAR(c: Char) 
  case ALT(r1: Rexp, r2: Rexp) 
  case SEQ(r1: Rexp, r2: Rexp) 
  case STAR(r: Rexp) 
}

import Rexp._

// some syntax sugar for regexes
import scala.language.implicitConversions

def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

// strings are coerced into Rexps
given Conversion[String, Rexp] = s => charlist2rexp(s.toList)

//val ABCD : Rexp = "abcd"

extension (r: Rexp) {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}



// marked regular expressions
enum REG {
  case BZERO
  case BONE
  case BCHAR(b: Boolean, c: Char)
  case BALT(r1: REG, r2: REG)
  case BSEQ(r1: REG, r2: REG)
  case BSTAR(r: REG)
  case BINIT(r: REG)
}

import REG._

// internalises a regular expression into a marked
// regular expression
def intern(r: Rexp) : REG = r match {
  case ZERO => BZERO
  case ONE => BONE
  case CHAR(c) => BCHAR(false, c)
  case ALT(r1, r2) => BALT(intern(r1), intern(r2))
  case SEQ(r1, r2) => BSEQ(intern(r1), intern(r2))
  case STAR(r) => BSTAR(intern(r))
}

// make sure the outermost REG is marked
def intern2(r: Rexp) : REG = BINIT(intern(r))

// flat function (removes any marking)
// translates a marked regex back into a standard regex
def fl(r: REG) : Rexp = r match {
  case BZERO => ZERO
  case BONE => ONE
  case BCHAR(b, d) =>  CHAR(d)
  case BALT(r1, r2) => ALT(fl(r1), fl(r2))
  case BSEQ(r1, r2) => SEQ(fl(r1), fl(r2))
  case BSTAR(r) => STAR(fl(r))
  case BINIT(r) => fl(r)
}

// nullable for marked regexes
def nullable(r: REG) : Boolean = r match {
  case BZERO => false
  case BONE => true
  case BCHAR(b, d) =>  false
  case BALT(r1, r2) => nullable(r1) || nullable(r2)
  case BSEQ(r1, r2) => nullable(r1) && nullable(r2)
  case BSTAR(r) => true
  case BINIT(r) => nullable(r)
}

// fin function from the paper
// checks whether a mark is in "final" position
def fin(r: REG) : Boolean = r match {
  case BZERO => false
  case BONE => false
  case BCHAR(b, _) => b
  case BALT(r1, r2) => fin(r1) || fin(r2)
  case BSEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case BSTAR(r) => fin(r)
}

// shift function from the paper
def shift(m: Boolean, r: REG, c: Char) : REG = r match {
  case BZERO => BZERO
  case BONE => BONE
  case BCHAR(_ , d) => BCHAR(m && d == c, d)
  case BALT(r1, r2) => BALT(shift(m, r1, c), shift(m, r2, c))
  case BSEQ(r1, r2) =>
    BSEQ(shift(m, r1, c), shift((m && nullable(r1)) || fin(r1), r2, c))
  case BSTAR(r) => BSTAR(shift(m || fin(r), r, c))
  case BINIT(r) => shift(true, r, c)
}

// ...when it encounters a BINIT-node it starts to
// shift marks into the REG


// the main matching function (by using BINIT only in 
// the first step a mark is shifted into the REG)
def mat(r: REG, s: List[Char]) : REG = s match {
  case Nil => r
  case c::cs => mat(shift(false, r, c), cs)
}

def matcher(r: REG, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s))



// pretty-printing REGs

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

def pp(e: REG) : String = e match {
  case BZERO => "0\n"
  case BONE => "1\n"
  case BCHAR(b, c) => if (b) s"•$c\n" else s"$c\n"
  case BALT(r1, r2) => "BALT\n" ++ pps(r1, r2)
  case BSEQ(r1, r2) => "BSEQ\n" ++ pps(r1, r2)
  case BSTAR(r) => "BSTAR\n" ++ pps(r)
  case BINIT(r) => "BINIT\n" ++ pps(r)
}
def pps(es: REG*) = indent(es.map(pp))




@main
def test1() = {
  println("=====Test====")
  val br1 = SEQ(ALT("a", "ab"), ALT("bc", "c"))
  val br2 = intern2(br1)
  val s = "abcd".toList
  println("=intern=")
  println(pp(br2) ++ "\n")
  println("=string=")
  println(s)
  println(s"=shift ${s(0)}=")
  println(pp(mat(br2, s.take(1))))

  println(s"=shift ${s(1)}=")
  println(pp(mat(br2, s.take(2))))

  println(s"=shift ${s(2)}=")
  println(pp(mat(br2, s.take(3))))

  println(s"=shift ${s(3)}=")
  println(pp(mat(br2, s.take(4))))
}

@main
def test2() = {
  println("=====Test====")
  val rexp = SEQ(ALT("a", "ab"), ALT("bc", ZERO))
  val breg = intern2(rexp)
  val str1 = "abc".toList
  val str2 = "abcd".toList
  println(matcher(breg, str1))
  println(matcher(breg, str2))
  println(pp(breg))
}

@main
def test3() = {
  println("=====Test====")
  val rexp = ("a" ~ "b") ~ "c"
  val breg = intern2(rexp)
  val str1 = "abc".toList
  println(s"start: $rexp")
  println("=============")
  for (n <- (1 to str1.length)) {
    val sl = str1.slice(0, n)
    println(s"shift: ${sl.last}")
    println(pp(mat(breg, sl)))
  }
}













/*
def indent ( s : String ) = s.split("\n").toList match {
  case h :: t => ( ("- " + h) +: t.map{"| " + _} ) mkString "\n"
  case _ => "- "
}

def treeString(a: Any) : String = a match {
          case x : Iterable[_] => {
            x.getClass.getSimpleName + ":\n" +
            x.toList.map{ s => treeString(s) }
              .map{ s => indent(s) }
              .mkString("\n")
          }
          case x : Product if x.productArity == 0 => 
            x.productPrefix
          case x : Product => {
            x.getClass.getSimpleName + ":\n" +
            x.productIterator
              .map{ s => treeString(s) }
              .map{ s => indent(s) }
              .mkString("\n")
          }
          case null => "null"
          case _ => "test : " + a.toString 
        }


extension (s1: String) {
  def +++(s2: String) = {
    val ss1 = explode(s1)
    val ss2 = explode(s2)
    val l1 = longest(ss1)
    val l2 = longest(ss2)
    implode(combine(ss1, ss2).map{(p1, p2) =>  p1.padR(l1) ++ p2.padR(l2)})
  }
  def padR(n: Int) = s"%1$$-${n}s".format(s1)
  def padL(n: Int) = s"%1$$${n}s".format(s1)
}

def longest(ss: List[String]) = ss.maxBy(_.length).length
def combine(ss1: List[String], ss2: List[String]) = ss1.zipAll(ss2, "", "")


*/