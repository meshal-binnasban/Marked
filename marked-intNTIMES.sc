import scala.compiletime.ops.string

enum Rexp {
  case ZERO
  case ONE 
  case CHAR(c: Char , marked: List[Int] = List(0)) //marked: Int = 0
  case ALT(r1: Rexp, r2: Rexp )
  case SEQ(r1: Rexp, r2: Rexp)
  case STAR(r: Rexp)
  case NTIMES(r: Rexp, n: Int,nmark:List[Int] = List(0) , counter:Int=0) 
  case INIT(r: Rexp)
  
  override def toString: String = this match {
    case ZERO => "ZERO"
    case ONE => "ONE"
    case CHAR(c, marked) => s"CHAR($c , $marked)"
    case ALT(r1, r2) => s"ALT($r1, $r2)"
    case SEQ(r1, r2) => s"SEQ($r1, $r2)"
    case STAR(r) => s"STAR($r)"
    case NTIMES(r, n, nmark ,counter) => s"NTIMES($r, n=$n, nmark=$nmark , count=$counter)"
    case INIT(r: Rexp) => s"INIT($r)"
  }
}
import Rexp._

def OPT(r: Rexp) = ALT(r, ONE)

def shift(mark: Int,re: Rexp,c: Char ): Rexp = re match {
    case ZERO => ZERO
    case ONE=> ONE
    case CHAR(ch,marked) => {
     // CHAR(ch, if(ch==c) 1 * mark else 0 * mark )
    val newMark = if (ch == c) 1 * mark else 0 * mark
    CHAR(ch, newMark :: marked) 
    }
    case ALT(r1, r2) => ALT(shift(mark,r1,c),shift(mark,r2,c))
    case SEQ(r1,r2) =>
      if (mark == 1 && nullable(r1)==1)
      {
      //println("if case")
        ALT(SEQ(shift(mark, r1, c), shift(fin(r1), r2, c)), shift(1, r2, c))
      }
      else{
      //println("else case")
        SEQ(shift(mark, r1, c), shift(fin(r1), r2, c))   
      }  
    //  SEQ (shift(mark,r1,c), shift(mark * nullable(r1) + fin(r1),r2,c))
    case STAR(r) => STAR(shift(mark + fin(r), r,c))
    case NTIMES(r, n,nmark,counter) =>
      if (counter == n || n==0) NTIMES(r, n,nmark,counter) else 
                    if (mark ==1 || fin(r)==1)
                    { val m=mark + fin(r)
                      NTIMES(shift(m, r, c), n , m::nmark,counter+1)
                    }
                    else NTIMES(shift(0, r, c), n,0::nmark,counter)
      /*
      if (n == 0) NTIMES(r, n,nmark,counter) else 
                    if (mark ==1 || fin(r)==1)
                    { val m=mark + fin(r)
                      NTIMES(shift(m, r, c), n - 1 , m::nmark,counter+1)
                    }
                    else NTIMES(shift(0, r, c), n,0::nmark,counter)
        */
    case INIT(r) => shift(1, r, c)
     
}

def nullable(r: Rexp) : Int = r match {
  case ZERO => 0
  case ONE => 1
  case CHAR(_,_) => 0
  case ALT(r1, r2) => nullable(r1) + nullable(r2)
  case SEQ(r1, r2) => nullable(r1) * nullable(r2)
  case STAR(_) => 1
  case NTIMES(r, n ,nmark , counter ) => if (n == 0) 1 else nullable(r) //?
  case INIT(r) => nullable(r)
}

def fin(r: Rexp) : Int = r match {
  case ZERO => 0
  case ONE => 0
  case CHAR(c,marked) => marked.head
  case ALT(r1, r2) => fin(r1) + fin(r2)
  case SEQ(r1, r2) => (fin(r1) * nullable(r2)) + fin(r2) 
  case STAR(r) => fin(r)
  case NTIMES(r,n,nmark , counter) => // we check the first n marks in nmarks, and if they each more than one and not more than n
    if(n == counter ) fin(r) else 0 
   // if(nmark >= n) fin(r) else 0 // 
}

def mat(r: Rexp, s: List[Char]) : Rexp = s match {
  case Nil => r
  case c::cs => mat(shift(0, r, c), cs)
}

def matcher(r: Rexp, s: List[Char]) : Int =
  if (s == Nil) nullable(r) else fin(mat(r, s))


def matcher2(r: Rexp, s: List[Char]) : Rexp =
  val x=mat(r, s)
  x match {
    case INIT(r) => 
      //println(s"Final Reg=$r")
      r
    case _ => 
     // println(s"Final Reg=$x")
      x
  }
  
enum VALUE {
  case ZEROV
  case EMPTY
  case CHARV(c: Char)
  case UNMARKED(s:String)
  case SEQV(v1: VALUE, r2: VALUE )
  case LEFT(v: VALUE)
  case RIGHT(v: VALUE)
  case STARV(vs: List[VALUE])
}
import VALUE._

def mkeps_marked(r: Rexp): VALUE = r match {
    case ONE => EMPTY
    case CHAR(c,marked) => if(marked.head >=1) CHARV(c)
      else UNMARKED(c.toString())
    case ALT(r1, r2) =>
      if (fin(r1) == 1) LEFT(mkeps_marked(r1))
      else if (fin(r2) == 1) RIGHT(mkeps_marked(r2))
      else UNMARKED("ALT")
    case SEQ(r1, r2) => SEQV(mkeps_marked(r1), mkeps_marked(r2))
    case STAR(r1) =>
      if (fin(r1) == 1) STARV(List(mkeps_marked(r1)))
      else STARV(Nil)
    case ZERO => ZEROV
  }

  //construct function which should generate a marked regex indicating 
  //the marked regular expression at the point of each input charachter (stage)
  //it should produce a list of r

def extractStage(r: Rexp, stage: Int): Rexp = r match {
  case ZERO => ZERO
  case ONE  => ONE
  case CHAR(c, marks) =>
    val m = marks(stage)  //if (marks.size > stage) marks(stage) else 0
    CHAR(c, List(m))
  case ALT(r1, r2) =>
    ALT(extractStage(r1, stage), extractStage(r2, stage))
  case SEQ(r1, r2) =>
    SEQ(extractStage(r1, stage), extractStage(r2, stage))
  case STAR(r1) =>
    STAR(extractStage(r1, stage))
  case NTIMES(r, n, nmark,counter) =>
  val m = nmark(stage) // if (nmark.size > stage) nmark(stage) else 0
    NTIMES(extractStage(r, stage), n, List(m),counter)
  case INIT(r) =>
    INIT(extractStage(r, stage))
}

def extractStages(finalReg: Rexp, input: List[Char]): List[(Char, Rexp)] = {
 // (0 to input.length-1).toList.map(stage => extractStage(finalReg, stage))
  val n = input.length
  (0 until n).toList.reverse.map { stage =>
    // For stage i, the corresponding input char is at position n - i - 1.
   (input(n - stage - 1),(extractStage(finalReg, stage) ) )
  }
}


def intern2(r: Rexp) : Rexp = INIT(r)


@main
def test00() = {
  
  val a=CHAR('a')
  val b=CHAR('b')
  //val rexp=ALT( SEQ(a, ALT(ONE, b)) , SEQ(a , b))
  val rexp=STAR(   ALT(a ,ALT(a ,b))   )

  val ss="ab".toList
  //val result=matcher(intern2(rexp),ss)
 // println("Testing mkeps \n")
 // println(s" rexp=$rexp Result=$result\n")

  val finReg=matcher2(intern2(rexp),ss)
 // println(mkeps_marked(finReg))

  // test extract
  val regsList=extractStages(finReg,ss)
  
  for ((ch,regex) <- regsList) {
  println(s"\nInput Character: $ch")
  println(s"Regex: $regex \n")
}

/*
  println("=====Test====")
  val br1 = intern2(rexp)
  println(s"ss is $ss \n rexp= $br1 \n ")

  for (i <- ss.indices) {
  println(s"${i + 1}- =shift ${ss(i)}=")
  val sPart = ss.take(i + 1)
  println(pp(mat(br1, sPart)))
  }
  println(matcher(br1,ss))
*/
}
@main
def test01() = {
    val n=2
    val rexp=intern2(EVIL1(n))

    val ss="aaab".toList
    println(s"input is $ss \n")

    println(s"NTIMES :\n")
    println(s"\n Original NTIMES\n ${pp(rexp)}")

    val result=matcher(rexp,ss)
    val finReg=matcher2(rexp,ss)

    println(s"\n Final NTIMES\n ${pp(finReg)}")
    println(s"\n NTIMES Result=$result and Size=${size(finReg)} \n \n")

   // val regsList=extractStages(finReg,ss)

    //for ((ch,regex) <- regsList) {
    //  println(s"\nInput Character: $ch")
    //  println(s"Regex: $regex \n")
   // }

    println("\n========================\n")

    val a=CHAR('a')
    val opta=ALT(a,ONE)

    val testRexp=intern2(SeqNTIMES(EVIL1(n)))
    println(s"input is $ss \n")

    println(s"Original SEQ :\n")
    println(s"\n ${pp(testRexp)}")

    val resultSeq=matcher(testRexp,ss)
    val finReg2=matcher2(testRexp,ss)

    println(s"\n Final SEQ: \n ${pp(finReg2)}")
    println(s"\n SEQ rexp :Result=$resultSeq Size=${size(finReg2)} \n\n")
/*
    val regsList=extractStages(finReg2,ss)

    for ((ch,regex) <- regsList) {
      println(s"\nInput Character: $ch")
      println(s"Regex: $regex \n")
      println(pp(regex))
    }
*/

}

@main
def test02() = {

  val a=CHAR('a')
  val b=CHAR('b')
  val opta=ALT(a,ONE)
  val optb=ALT(b,ONE)
  val newS="a".toList

  //SEQ(NTIMES(OPT(CHAR('a')), n), NTIMES(CHAR('a'), n))
  val newRexp=SEQ(opta,opta) 
  val newRexpI= intern2(newRexp)
  println(s"newS is $newS \n rexp= $newRexpI \n ")
  for (i <- newS.indices) {
  println(s"${i + 1}- =shift ${newS(i)}=")
  val sPart = newS.take(i + 1)
  println(pp(mat(newRexpI, sPart)))
  }
  println(matcher(newRexpI,newS))


  println("\n========================\n")

  val sequence=SEQ(SEQ(opta,ONE),a)
   
  val sequenceT=intern2(sequence)
  val finReg3=matcher2(intern2(sequenceT),"a".toList)
  println(pp(finReg3))
  println(s"The result is :${fin(finReg3)}")

}

@main
def test03() = {
    val n=2
    val rexp=EVIL1(n)
    println("=====Test====")
    val br1 = intern2(rexp)
    val s = "aaab".toList
    println(s"s is $s \n rexp= $br1 \n ")

    for (i <- s.indices) {
    println(s"${i + 1}- =shift ${s(i)}=")
    val sPart = s.take(i + 1)
    println(pp(mat(br1, sPart)))
    }
    println(matcher(br1,s))
}

def size(r: Rexp) : Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_,_) => 1
  case ALT(r1, r2) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2) => 1 + size(r1) + size(r2)
  case STAR(r) => 1 + size(r)
  case NTIMES(r,n, nmark,counter) => 1 + size(r) 
  case INIT(r) => 1 + size(r)   
}

def SeqNTIMES(r: Rexp): Rexp = r match {
  case ZERO => ZERO
  case ONE  => ONE
  case CHAR(c, marked) => CHAR(c, marked)
  case ALT(r1, r2) =>
    ALT(SeqNTIMES(r1), SeqNTIMES(r2))
  case SEQ(r1, r2) =>
    SEQ(SeqNTIMES(r1), SeqNTIMES(r2))
  case STAR(r1) =>
    STAR(SeqNTIMES(r1))
  case NTIMES(r, n, nmark,counter) =>
    val expandR = SeqNTIMES(r)
    if (n <= 0) ONE else
        if (n == 1) expandR else 
            { SeqNTIMES( SEQ( expandR, NTIMES(expandR,n-1,nmark,counter) ) ) }
  case INIT(r) => ZERO
}

def EVIL1(n: Int) = SEQ(NTIMES(OPT(CHAR('a')), n), NTIMES(CHAR('a'), n))
val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

// for measuring time
def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

//@arg(doc = "Test (a?{n}) (a{n})")
@main
def test1() = {
  for (i <- 0 to 11000 by 1000) {
    println(f"$i: ${time_needed(2, matcher(EVIL1(i), ("a" * i).toList))}%.5f")
  }
}

//@arg(doc = "Test (a*)* b")
@main
def test2() = {
  for (i <- 0 to 7000000 by 500000) {
    println(f"$i: ${time_needed(2, matcher(EVIL2, ("a" * i).toList))}%.5f")
  }
} 

//@arg(doc = "All tests.")
@main
def all() = { test1(); test2() } 

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

def pp(e: Rexp) : String = e match {
  case ZERO => "0\n"
  case ONE => "1\n"
  case CHAR(c,marked) => if (marked.head>=1) s"•$c m:$marked\n" else s"$c m:$marked\n"
  case ALT(r1, r2) => "ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => "SEQ\n" ++ pps(r1, r2)
  case STAR(r) => "STAR\n" ++ pps(r) //if(n == 0 ) fin(r) 
  case NTIMES(r,n,nmark,counter) => if (n == 0 && fin(r) == 1) s"• NTIMES(n=$n) $nmark counter=$counter\n" ++ pps(r) 
    else s"NTIMES(n=$n) $nmark counter=$counter\n" ++ pps(r)
  case INIT(r) => "INIT\n" ++ pps(r)
}
def pps(es: Rexp*) = indent(es.map(pp))


/*
def matcher(r: Rexp , s: String) : Int = s.toList match {
  case Nil => nullable(r)
//case c :: cs => fin(cs.foldLeft(shift(true, r, c)) { (acc, c) =>
//  shift(false, acc, c) })
  case c :: cs => //fin(cs.foldLeft(shift(1, r, c))(shift(0, _, _)))
    val x=cs.foldLeft(shift(1, r, c))(shift(0, _, _))
    println(s"\n Last Reg: \n $x")
    fin(x)
}
*/