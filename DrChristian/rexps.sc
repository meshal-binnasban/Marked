import $file.rexp, rexp._

// ================= Rexp with ids =================
abstract class RexpS
case object ZEROS extends RexpS
case object ONES  extends RexpS
//case class CHARS(c: Char,id:Int) extends RexpS
case class CHARS(c: Char) extends RexpS
case class ALTS(r1: RexpS, r2: RexpS, id:Int) extends RexpS
case class SEQS(r1: RexpS, r2: RexpS, id: Int) extends RexpS
case class STARSS(r: RexpS, id: Int) extends RexpS
case class NTIMESS(r: RexpS, n: Int) extends RexpS
case class ANDS(r1: RexpS, r2: RexpS) extends RexpS

def intern(r: Rexp): RexpS = internalize(r, 0)._1

def internalize(r: Rexp, id: Int): (RexpS, Int) = r match {
  case ZERO => (ZEROS, id)
  case ONE  => (ONES, id)
  //case CHAR(c) => (CHARS(c,id), id + 1)
  case CHAR(c) => (CHARS(c), id)
  case ALT(r1, r2) =>
    val myId = id
    val (r1s, id1) = internalize(r1, id + 1)
    val (r2s, id2) = internalize(r2, id1)
    (ALTS(r1s, r2s, myId), id2)

  case SEQ(r1, r2) =>
    val myId = id
    val (r1s, id1) = internalize(r1, id + 1)
    val (r2s, id2) = internalize(r2, id1)
    (SEQS(r1s, r2s, myId), id2)

  case STAR(r0) =>
    val myId = id
    val (rs, id1) = internalize(r0, id + 1)
    (STARSS(rs, myId), id1)

  case NTIMES(r0, n) =>
    val (rs, id1) = internalize(r0, id)
    (NTIMESS(rs, n), id1)

  case AND(r1, r2) =>
    val (r1s, id1) = internalize(r1, id)
    val (r2s, id2) = internalize(r2, id1)
    (ANDS(r1s, r2s), id2)
}


def nullableS(r: RexpS): Boolean = r match {
  case ZEROS           => false
  case ONES            => true
  //case CHARS(_,_)        => false
  case CHARS(_)        => false
  case ALTS(r1, r2, _)    => nullableS(r1) || nullableS(r2)
  case SEQS(r1, r2, _) => nullableS(r1) && nullableS(r2)
  case STARSS(_, _)    => true
  case NTIMESS(r, n)   => n == 0 || nullableS(r)
  case ANDS(r1, r2)    => nullableS(r1) && nullableS(r2)
}

// ================= pretty-printing RexpS =================
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

def ppp(r: RexpS) : String = (r: @unchecked) match {
  case ZEROS => "0\n"
  case ONES  => "1\n"
  case CHARS(c) => s"$c\n"
 // case CHARS(c,id) => s"$c id=$id\n"
  case ALTS(r1, r2,id) =>
    s"ALT id=$id\n" ++ ppps(r1, r2)

  case SEQS(r1, r2, id) =>
    s"SEQ id=$id\n" ++ ppps(r1, r2)

  case STARSS(r, id) =>
    s"STAR id=$id\n" ++ ppps(r)

  case NTIMESS(r, n) =>
    s"NTIMES($n)\n" ++ ppps(r)

  case ANDS(r1, r2) =>
    "AND\n" ++ ppps(r1, r2)
}

def ppps(rs: RexpS*) = indent(rs.map(ppp))

def mkeps2(r: RexpS): Bits = r match {
  case ONES => Nil
  case ALTS(r1, r2, _) => if (nullableS(r1)) Lf :: mkeps2(r1) else Ri :: mkeps2(r2)
  case SEQS(r1, r2, _) => mkeps2(r1) ++ mkeps2(r2)
  case STARSS(_, _) => List(En)
  case NTIMESS(_, _) => List(EnT)
  case ANDS(r1, r2) => mkeps2(r1) ++ mkeps2(r2)
}


def mkeps(r: RexpS) : Val = r match {
  case ONES => Empty
  case ALTS(r1, r2, _) =>
    if (nullableS(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
  case SEQS(r1, r2, _) => Sequ(mkeps(r1), mkeps(r2))
  case STARSS(r,_) => Stars(Nil)
  case NTIMESS(r, n) => Nt(Nil, 0)
  case ANDS(r1,r2) => Sequ(mkeps(r1),mkeps(r2))
}



