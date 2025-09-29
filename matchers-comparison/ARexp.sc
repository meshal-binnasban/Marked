import $file.rexp, rexp._
//import $file.Val, Val._


// annotated regular expressions
abstract class ARexp 
case object AZERO extends ARexp
case class AONE(bs: Bits) extends ARexp
case class ACHAR(bs: Bits, c: Char) extends ARexp
case class AALTS(bs: Bits, rs: List[ARexp]) extends ARexp 
case class ASEQ(bs: Bits, r1: ARexp, r2: ARexp) extends ARexp 
case class ASTAR(bs: Bits, r: ARexp) extends ARexp 
//case class ANTIMES(bs: Bits, r: ARexp, n: Int) extends ARexp 


// an abbreviation for binary alternatives
def AALT(bs: Bits, r1: ARexp, r2: ARexp) = AALTS(bs, List(r1, r2))


//erase function: extracts the Rexp from ARexp
def erase(r:ARexp): Rexp = r match{
    case AZERO => ZERO
    case AONE(_) => ONE
    case ACHAR(bs, c) => CHAR(c)
    case AALTS(bs, Nil) => ZERO
    case AALTS(bs, a::Nil) => erase(a)
    case AALTS(bs, a::as) => ALT(erase(a), erase(AALTS(bs, as)))
    case ASEQ(bs, r1, r2) => SEQ (erase(r1), erase(r2))
    case ASTAR(cs, r)=> STAR(erase(r))
}

// adds (fuses) a bitsequence to an arexp
def fuse(bs: Bits, r: ARexp) : ARexp = r match {
  case AZERO => AZERO
  case AONE(cs) => AONE(bs ++ cs)
  case ACHAR(cs, c) => ACHAR(bs ++ cs, c)
  case AALTS(cs, rs) => AALTS(bs ++ cs, rs)
  case ASEQ(cs, r1, r2) => ASEQ(bs ++ cs, r1, r2)
  case ASTAR(cs, r) => ASTAR(bs ++ cs, r)
}

// internalises a "normal" regex into an annotated regex
def internalise(r: Rexp) : ARexp = r match {
  case ZERO => AZERO
  case ONE => AONE(Nil)
  case CHAR(c) => ACHAR(Nil, c)
  case ALT(r1, r2) => AALT(Nil, fuse(List(Z), internalise(r1)), fuse(List(S), internalise(r2)))
  case SEQ(r1, r2) => ASEQ(Nil, internalise(r1), internalise(r2))
  case STAR(r) => ASTAR(Nil, internalise(r))
}
