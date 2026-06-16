//| mainClass: Main
//| scalaVersion: 3.8.3
//| scalacOptions: ["-deprecation", "-feature", "-language:implicitConversions" , "-nowarn"]
//| mvnDeps: 
//|   - org.scala-lang:scala3-library_3:3.8.3
//|   - org.scala-lang.modules::scala-parallel-collections:1.2.0
//| moduleDeps: 
//|   - rexp.scala
//|   - enumerate.scala
//|   - regenerate.scala
//|   - re_bitrev3.scala
//|   - Generators.scala

import Rexp._

type Markss = Set[Set[Int]]

def mat(r: Rexp, s: String) : Markss = {
def shifts2(mss: Markss, r: Rexp) : Markss = r match {
case ZERO => Set()
case ONE => mss
case CHAR(c) =>
for (ms <- mss;
ms1 = for (m <- ms; if m < s.length && s(m) == c) yield m + 1;
if ms1.nonEmpty) yield ms1
case ALT(r1, r2) => shifts2(mss, r1) ++ shifts2(mss, r2)
case SEQ(r1, r2) => shifts2(shifts2(mss, r1), r2)
case STAR(r) => {
val mss1 = shifts2(mss, r).diff(mss)
if (mss1.isEmpty) mss else mss ++ shifts2(mss1, STAR(r))
}
case NTIMES(r1, n) =>
if (n == 0) mss
else shifts2(shifts2(mss, r1), NTIMES(r1, n - 1))
}
shifts2(Set(Set(0)), r)
}

def matcher(r: Rexp, s: String) : Boolean = {
if (s == "") nullable(r)
else mat(r, s).exists(_.contains(s.length))
}