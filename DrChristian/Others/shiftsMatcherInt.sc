
//   

import scala.language.implicitConversions

import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._

type Marks = Set[Int]

// shifts function 
def shifts(ms: Marks, s: String, r: Rexp) : Marks = r match {
  case ZERO => Set()
  case ONE => Set()
  case CHAR(c) => for (m <- ms; if m < s.length && s(m) == c) yield (m + 1)
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
    if(ms1.isEmpty)
     Set()
    else
        ms1 ++ shifts(ms1, s,STAR(r))
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
  shifts(Set((0)), s, r)

def matcher(r: Rexp, s: String) : Boolean = {
  if (s == "") nullable(r)
  else mat(r, s).exists(_ == s.length)
}

@main
def test1() = {
  val reg = %("a")
  val s="a"
  println(s"$s: ${matcher(reg, s)}  ${mat(reg, s)} ")
  
}
@main
def test2() = {
  val reg = %("a"|"aa")
  val s="aaa"
  println(s"$s: ${matcher(reg, s)}  ${mat(reg, s)} ")
  
}

