error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Play.sc:`<none>`.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Play.sc
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -rexp/shift.
	 -rexp/shift#
	 -rexp/shift().
	 -shift.
	 -shift#
	 -shift().
	 -scala/Predef.shift.
	 -scala/Predef.shift#
	 -scala/Predef.shift().
offset: 1165
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Play.sc
text:
```scala
import $file.rexp, rexp._

def fin(r: Rexp) : Boolean = (r: @unchecked) match {
  case ZERO => false
  case ONE => false
  case CHAR(_) => false
  case POINT(_, CHAR(_)) => true
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
}

def shift(m: Boolean, bs: Bits, r: Rexp, c: Char) : Rexp = (r: @unchecked) match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) => if (m && d == c) POINT(bs, CHAR(d)) else CHAR(d)
  case POINT(_, CHAR(d)) => if (m && d == c) POINT(bs, CHAR(d)) else CHAR(d)
  case ALT(r1, r2) => ALT(shift(m, bs :+ Z, r1, c), shift(m, bs :+ S, r2, c))
  case SEQ(r1, r2) if m && nullable(r1) => SEQ(shift(m, bs, r1, c), shift(true, bs ::: mkeps(r1), r2, c))
  case SEQ(r1, r2) if fin(r1) => SEQ(shift(m, bs, r1, c), shift(true, mkfin(r1), r2, c))
  case SEQ(r1, r2) => SEQ(shift(m, bs, r1, c), shift(false, Nil, r2, c))
  case STAR(r) if m && fin(r) => STAR(shift(true, bs ::: (mkfin(r) :+ Z), r, c))
  case STAR(r) if fin(r) => STAR(shift(true, mkfin(r) :+ Z, r, c)) 
  case STAR(r) if m => STAR(shift(m, bs, r, c))
  case STAR(r) => STAR(shif@@t(false, Nil, r, c))
}


// the main matching function (by using BINIT only in 
// the first step a mark is shifted into the Rexp)
def mat(r: Rexp, s: List[Char]) : Rexp = s match {
  case Nil => r
  case c::cs => cs.foldLeft(shift(true, Nil, r, c))((r, c) => shift(false, Nil, r, c))
}

def matcher(r: Rexp, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s))

def lex(r: Rexp, s: List[Char]) : Option[Bits] = {
  if matcher(r, s)
  then Some(if (s == Nil) mkeps(r) else mkfin(mat(r, s)))
  else None
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
  case ONE => "1\n"
  case CHAR(c) => s"$c\n"
  case POINT(bs, CHAR(c)) => s"•$c:${bs.mkString(",")}\n" 
  case ALT(r1, r2) => "ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => "SEQ\n" ++ pps(r1, r2)
  case STAR(r) => "STAR\n" ++ pps(r)
}
def pps(es: Rexp*) = indent(es.map(pp))




@main
def test1() = {
  println("=====Test====")
  val br2 = ("a" | "ab") ~ ("c" | "bc")
  val s = "abcd".toList
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

  println(s"=final list=")
  println(lex(br2, s.take(4)))
}

@main
def test2() = {
  println("=====Test====")
  //val br2 = SEQ(CHAR('a'),ALT(ONE,ONE))
  val br2 = SEQ(ALT(ONE,CHAR('c')),ALT(SEQ(CHAR('c'),CHAR('c')),CHAR('c')))
  val s = "cc".toList
  println("=string=")
  println(s)
  println(s"=shift ${s(0)}=")
  println(pp(mat(br2, s.take(1))))

  println(s"=shift ${s(1)}=")
  println(pp(mat(br2, s.take(2))))

  /*
  println(s"=shift ${s(2)}=")
  println(pp(mat(br2, s.take(3))))

  println(s"=shift ${s(3)}=")
  println(pp(mat(br2, s.take(4))))
  */
  println(s"=final list=")
  println(lex(br2, s.take(3)))
}

@main
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
}






```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.