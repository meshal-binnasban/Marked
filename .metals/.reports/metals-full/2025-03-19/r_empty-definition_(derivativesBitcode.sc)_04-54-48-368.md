error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/derivativesBitcode.sc:90
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/derivativesBitcode.sc
empty definition using pc, found symbol in pc: 
semanticdb not found
|empty definition using fallback
non-local guesses:
	 -BONE.
	 -BONE#
	 -BONE().
	 -scala/Predef.BONE.
	 -scala/Predef.BONE#
	 -scala/Predef.BONE().

Document text:

```scala
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 
case class NTIMES(r: Rexp, n: Int) extends Rexp 

abstract class BRegExp
case class BZERO() extends BRegExp
case class BONE(bs: List[Int]) extends BRegExp
case class BCHAR(bs: List[Int], c: Char) extends BRegExp
case class BALTs(bs: List[Int], rs: List[BRegExp]) extends BRegExp
case class BSEQ(bs: List[Int], r1: BRegExp, r2: BRegExp) extends BRegExp
case class BSTAR(bs: List[Int], r: BRegExp) extends BRegExp
case class BNTIMES(bs: List[Int], r: BRegExp, n: Int) extends BRegExp

// Convert standard regex to bitcoded regex

def fuse(bs: List[Int], r: BRegExp): BRegExp = r match {
  case BZERO() => BZERO()
  case BONE(bs2) => BONE(bs ++ bs2)
  case BCHAR(bs2, c) => BCHAR(bs ++ bs2, c)
  case BALTs(bs2, rs) => BALTs(bs ++ bs2, rs)
  case BSEQ(bs2, r1, r2) => BSEQ(bs ++ bs2, r1, r2)
  case BSTAR(bs2, r) => BSTAR(bs ++ bs2, r)
  case BNTIMES(bs2, r, n) => BNTIMES(bs ++ bs2, r, n)
}

def internalize(r: Rexp): BRegExp = r match {
  case ZERO => BZERO()
  case ONE => BONE(List())
  case CHAR(c) => BCHAR(List(), c)
  case ALT(r1, r2) => BALTs(List(), List(fuse(List(0), internalize(r1)), fuse(List(1), internalize(r2))))
  case SEQ(r1, r2) => BSEQ(List(), internalize(r1), internalize(r2))
  case STAR(r) => BSTAR(List(), internalize(r))
  case NTIMES(r, n) => BNTIMES(List(), internalize(r), n)
}

// Nullable for Bitcoded Regex
def bnullable(r: BRegExp): Boolean = r match {
  case BZERO() => false
  case BONE(_) => true
  case BCHAR(_, _) => false
  case BALTs(_, rs) => rs.exists(bnullable)
  case BSEQ(_, r1, r2) => bnullable(r1) && bnullable(r2)
  case BSTAR(_, _) => true
  case BNTIMES(_, r, n) => if (n == 0) true else bnullable(r)
}

// Compute derivative with Bitcodes
def bder(c: Char, r: BRegExp): BRegExp = r match {
  case BZERO() => BZERO()
  case BONE(_) => BZERO()
  case BCHAR(bs, d) => if (c == d) BONE(bs) else BZERO()
  case BALTs(bs, rs) => BALTs(bs, rs.map(bder(c, _)))
  case BSEQ(bs, r1, r2) => 
    if (bnullable(r1)) BALTs(bs, List(BSEQ(List(), bder(c, r1), r2), fuse(bmkeps(r1), bder(c, r2))))
    else BSEQ(bs, bder(c, r1), r2)
  case BSTAR(bs, r) => BSEQ(bs :+ 0, bder(c, r), BSTAR(List(), r))
  case BNTIMES(bs, r, n) => 
    if (n == 0) BZERO()
    else BSEQ(bs :+ 0, bder(c, r), BNTIMES(List(), r, n - 1))
}

def bders(s: List[Char], r: BRegExp): BRegExp = s match {
  case Nil => r
  case c :: cs => bders(cs, bder(c, r))
}

// Compute bmkeps - extracts the bitcode encoding of the POSIX value
def bmkeps(r: BRegExp): List[Int] = r match {
  case BONE(bs) => bs
  case BALTs(bs, rs) => bs ++ bmkepss(rs)
  case BSEQ(bs, r1, r2) => bs ++ bmkeps(r1) ++ bmkeps(r2)
  case BSTAR(bs, _) => bs :+ 1
  case BNTIMES(bs, r, n) => 
    if (n == 0) bs :+ 1
    else bs ++ List(0) ++ bmkeps(r) ++ bmkeps(BNTIMES(List(), r, n - 1))
}

def bmkepss(rs: List[BRegExp]): List[Int] = rs match {
  case Nil => Nil
  case r :: rs => if (bnullable(r)) bmkeps(r) else bmkepss(rs)
}

// Decode function to extract the POSIX value from the bitcoded regex
def decode(bs: List[Int], r: Rexp): Option[String] = r match {
  case ZERO() => None
  case BONE(_) => Some("")
  case BCHAR(_, c) => Some(c.toString)
  case BALTs(_, rs) => rs.view.flatMap(decode(bs, _)).headOption
  case BSEQ(_, r1, r2) => for {
    s1 <- decode(bs, r1)
    s2 <- decode(bs, r2)
  } yield s1 + s2
  case BSTAR(_, r) => Some("*")
  case BNTIMES(_, r, _) => decode(bs, r)
}

// Bitcoded Matcher Function
def bmatcher(r: Rexp, s: String): Boolean = {
  val br = internalize(r)
  val bderiv = s.foldLeft(br)((acc, c) => bder(c, acc))
  bnullable(bderiv)
}

// Run tests



@main
def test1() = {
    val regex = ALT(CHAR('a'), CHAR('b'))
    val s="a"

    val finalReg=bders(s.toList, internalize(regex))
    println(finalReg)
    println( bmkeps(finalReg))
    print(decode(bmkeps(finalReg), regex))
}
```

#### Short summary: 

empty definition using pc, found symbol in pc: 