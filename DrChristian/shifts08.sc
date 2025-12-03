// simple test using SETS instead of list of marks
//
// => 26 mins (for 100_000_000)
// => not much change from the list version for 10 strings
//   
// 53 mins vs 85 mins for 20 strings and 100_000_000

import scala.language.implicitConversions
/* //> using file rexp.sc 
//> using file enumerate.sc
//> using file regenerate.sc */
import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.rebit

type Marks = Set[(Int, Int)]
// shifts function 
def shifts(ms: Marks, s: String, r: Rexp) : Marks = r match {
  case ZERO => Set()
  case ONE => Set()
  case CHAR(c) => for ((n, m) <- ms; if m < s.length && s(m) == c) yield (n, m + 1)
  case ALT(r1, r2) => shifts(ms, s, r1) ++ shifts(ms, s, r2)
  case SEQ(r1, r2) => {
    val ms1 = shifts(ms, s, r1)
    (nullable(r1), nullable(r2)) match {
      case (true, true) =>  shifts(ms1 ++ ms, s, r2) ++ ms1
      case (true, false) => shifts(ms1 ++ ms, s, r2) 
      case (false, true) => shifts(ms1, s, r2) ++ ms1
      case (false, false) => shifts(ms1, s, r2)
    }   
  }
  case STAR(r) => {
    val ms1 = shifts(ms, s, r)
    if(ms1.isEmpty) Set()  
      else{
        ( (ms1) ++ shifts(ms1, s, STAR(r))   )
      }
  }
  case NTIMES(r, n) =>
    if (n == 0) Set()              
    else if (n == 1) shifts(ms, s, r)
    else {
      val ms1 = shifts(ms, s, r)
      if (ms1 == Set()) Set()
      else
      if (nullable(r)) ms1 ++ shifts(ms1, s, NTIMES(r, n - 1))
      else shifts(ms1, s, NTIMES(r, n - 1))
    }
}

// the main matching function 
def mat(r: Rexp, s: String) : Marks = 
  shifts(Set((0, 0)), s, r)

def matcher(r: Rexp, s: String) : Boolean = {
  if (s == "") nullable(r)
  else
    val ms=mat(r,s)
    mat(r, s).exists(_._2 == s.length)
}
 
def lexer(r: Rexp, s: String): Val = {
  val invalidPair = (-1, -1)

  if (s == "") {
    if (nullable(r)) Empty else Invalid
  } else {
    val ms     = mat(r, s)
    val finals = ms.filter(_._2 == s.length)

    if (finals.isEmpty) Invalid
    else {
      val best = finals.head          
      val (v, p0) = back(r, s, best)  
      if (p0 == invalidPair) Invalid
      else 
        //println(s"returned p=$p0")
        v
    }
  }
}

@main
def test1() = {
  val reg = ("a" | ("aa") )
  val s="aa"
  println(s"$s: ${matcher(reg, s)}  ${mat(reg, s)} ")
  //println(back(reg,s,(0,1))._1)
  println(lexer(reg,s))
  println(rebit.blexer(reg,s))

}

def back(r: Rexp, s: String, p: (Int, Int)): (Val, (Int, Int)) = {
  val invalid = (-1, -1)

  r match {
    case ONE =>
      val (n, m) = p
      if (n == m) (Empty, (n, m)) else (Invalid, invalid)
    case CHAR(c) =>
      val (n, m) = p
      if (m > n && m <= s.length && s(m - 1) == c)
        //println(s"${Chr(c)}");
        (Chr(c), (n, m - 1))
      else
        (Invalid, invalid)

    case ALT(r1, r2) =>
      val (vL, pL) = back(r1, s, p)
      val (vR, pR) = back(r2, s, p)
      val m  = p._2
      val dL = if (pL == invalid) -1 else m - pL._2
      val dR = if (pR == invalid) -1 else m - pR._2

      if (dL < 0 && dR < 0)
        (Invalid, invalid)
      else if (dL > dR)
        //println(s"${Left(vL)}");
        (Left(vL), pL)
      else if (dR > dL)
        //println(s"${Right(vR)}");
        (Right(vR), pR)
      else
        //println(s"${Left(vL)}");
        (Left(vL), pL)
    
    case SEQ(r1, r2) =>
      val m = p._2

      // 1- r1 consumed
      val (v1All, p1All) = back(r1, s, p)
      val cand1: (Val, (Int, Int)) =
        if (p1All == invalid || !nullable(r2)) (Invalid, invalid)
        else {
          val i = p1All._2
          val (v2Empty, _) = back(r2, s, (i, i))
          (Sequ(v1All, v2Empty), p1All)
        }

      // 2- r1 then r2
      val cand2: (Val, (Int, Int)) = {
        val (v2, pMid) = back(r2, s, p)
        if (pMid == invalid) (Invalid, invalid)
        else {
          val (v1, pPrev) = back(r1, s, pMid)
          if (pPrev == invalid) (Invalid, invalid)
          else (Sequ(v1, v2), pPrev)
        }
      }

      // 3) r2 consumed 
      val (v2All, p2All) = back(r2, s, p)
      val cand3: (Val, (Int, Int)) =
        if (p2All == invalid || !nullable(r1)) (Invalid, invalid)
        else 
          {
            val i = p2All._1
            val (v1Empty, _) = back(r1, s, (i, i))
            (Sequ(v1Empty, v2All), p2All)
          }

      val cands = List(cand1, cand2, cand3)

      def delta(pair: (Int, Int)): Int =
        if (pair == invalid) -1 else m - pair._2

      val (bestV, bestP) =
        cands.reduce { case ((vA, pA), (vB, pB)) =>
          val dA = delta(pA)
          val dB = delta(pB)
          if (dA > dB) (vA, pA)
          else if (dB > dA) (vB, pB)
          else (vA, pA) 
        }

      if (bestP == invalid) (Invalid, invalid)
      else {
        //println(s"$bestV")
        (bestV, bestP)
      }
    
    case _ =>
      (Invalid, invalid)
  }
}


@main
def testall() = {
  given rexp_cdata : CDATA[Rexp] = List(
        (0, _ => ONE),
        (0, _ => ZERO),
        (0, _ => CHAR('a')),
        (0, _ => CHAR('b')),
        (0, _ => CHAR('c')),
       // (1, cs => STAR(cs(0))),
        (2, cs => ALT(cs(0), cs(1))),
        (2, cs => SEQ(cs(0), cs(1)))
      )
  val alphabet = LazyList('a', 'b')

  for (i <- 0L to 100_000_000L) {
    val r = decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- (generate_up_to(alphabet)(20)(r).take(19)) if s != "") {
      
      val vm=lexer(r,s)
      val vb=rebit.blexer(r,s)
      val res = vm == vb
      if (!res) {
        println(s"$r and $s")
        println(s"vm=$vm vb=$vb")
        System.exit(1)
      }




    }
  }
}


// for measuring time
def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

def mkstar(n: Int) = STAR("a" * n)
def mkalts(n: Int) = {
  (for (i <- (1 to n).toList) yield mkstar(i)).reduceLeft(ALT.apply)
}

def snds(ms: Marks) = ms.map((_, m) => (m, m))

def advance(ms: Marks, subs: Set[String], s: String) : Marks = {
    val ms1 = (for ((n, m) <- ms; ss <- subs;
                if m + ss.length <= s.length;
                if s.substring(m, m + ss.length) == ss) yield (n, m + ss.length))
    if (ms1 == Set()) Set() else ms1 ++ advance(ms1, subs, s)            
}