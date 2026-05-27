//| scalaVersion: 3.8.3
//| scalacOptions: ["-deprecation", "-feature"]
//| mvnDeps: 
//|   - org.scala-lang:scala3-library_3:3.8.3
//| moduleDeps: 
//|   - rexp.scala
//|   - enumerate.scala
//|   - regenerate.scala

import Rexp._

type Marks = Set[Int]


def mat(r: Rexp, s: String) : Marks = {
    def shifts2(ms: Marks, r: Rexp) : Marks = r match {
      case ZERO => Set()
      case ONE => ms
      case CHAR(c) => for (m <- ms; if m < s.length && s(m) == c) yield m + 1
      case ALT(r1, r2) => shifts2(ms, r1) ++ shifts2(ms, r2)
      case SEQ(r1, r2) => shifts2(shifts2(ms, r1), r2)
      case STAR(r) => {
        val ms1 = shifts2(ms, r).diff(ms)
        if (ms1 == Set()) ms else ms ++ shifts2(ms1, STAR(r)) 
      }
      case NTIMES(r1, n) =>
        if (n == 0) ms 
        else shifts2(shifts2(ms, r1), NTIMES(r1, n - 1))

      case NOT(r) =>
        ms.flatMap { m =>
          val allFromM = Range(m, s.length + 1).toSet
          val rFromM = shifts2(Set(m), r)
          allFromM.diff(rFromM)
        }
    
    }

    shifts2(Set(0), r)
}

def matcher(r: Rexp, s: String) : Boolean = {
  if (s == "") nullable(r)
  else mat(r, s).exists(_ == s.length)
}

/*
val r1 = ALT("aa", "a")
val r2 = STAR(ALT(ONE, "aaaa"))    
val r = SEQ(SEQ(r1, r2), "aaaa")
mat(r, "aaaaaaaaa")
mat(r, "aaaaaaaaa")
matcher(r, "aaaaaaaaa")
matcher(r, "aaaaaaaaa")

val r3=ALT(ONE , NOT("a"))
val r4=SEQ(ALT(ONE, "a"), NOT("a"))
*/


//def main(args: Array[String]): Unit = ()
