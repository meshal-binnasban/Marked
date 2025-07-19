// SEQ case Tests
//(ONE | "a" ) ~ ( "a" | "aa" )
@main 
def test1() = {
  println("=====Test====")
  //val br2 = (ONE | "a" ) ~ ( "a" | "aa" )
  val br2= (ONE | "a") ~ ("a" | "aa")
  val s = "a"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")    
}

//("a" | "ab") ~ ("c" | "bc")
@main
def test2() = {
  println("=====Test====")
  val br2= ("a" | "ab") ~ ("c" | "bc")
    //%("aa") | ("aa" ~ ONE)
  val s = "abc"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")  
}

//(("a"|"b") | ("ab")) ~ (("bc") | ("c" | "b"))
@main
def test3() = {
  println("=====Test====")
  val br2= (("a"|"b") | ("ab")) ~ (("bc") | ("c" | "b"))
    //%("aa") | ("aa" ~ ONE)
  val s = "abc"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s) 
  println(matcher(br2,s))

  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")  
  val result=matcher(br2, s)
  println(result) 
}

//( ONE | ( ONE | "bc" )  )  | ( "a"| ONE ) ~ ("a" | "aa")
@main
def test4() = {
  println("=====Test====")
  val br2= ( ONE | ( ONE | "bc" )  )  | ( "a"| ONE ) ~ ("a" | "aa")
  val s = "aa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")    
}

//( ("a" | "b") ~ (ONE | "a") ) ~ "a"
@main
def test5() = {
  println("=====Test====")
  val br2= "ab"
  //( ("a" | "b") ~ (ONE | "a") ) ~ "a"
  val s = "ab"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}") 
}

//((ONE | "a") ~ (ONE | "a"))
@main
def test6() = {
  println("=====Test====")
  //val br2=((ONE | "a") ~ (ONE | "a"))
  val br2=((ONE | "a") ~ (ONE | "a"))
  
  val s = "aa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")  
  val result=matcher(br2, s)
  println(result)
}

//(((ONE | "a") ~ (ZERO | "a")) | (ONE | (ONE | "a")))
@main
def test7() = {
  println("=====Test====")
  val br2=(((ONE | "a") ~ (ZERO | "a")) | (ONE | (ONE | "a")))
  
  val s = "a"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")  
  val result=matcher(br2, s)
  println(result)
}

//(ONE | (ONE | (ONE | "a"))) | (((ZERO | ONE) | ("a" | ZERO)) | (ONE | "aa"))
@main
def test8() = {
  println("=====Test====")
  val br2=((ONE | "a") | "aa")
    //(ONE | (ONE | (ONE | "a"))) | (((ZERO | ONE) | ("a" | ZERO)) | (ONE | "aa"))
  
  val s = "aa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")  
  val result=matcher(br2, s)
  println(result)
}

//(ONE | "a") | (("a" | "aa") ~ (ONE | "a"))
@main
def test9() = {
  println("=====Test====")
  val br2=(("a" | "aa") ~ (ONE | "a"))
  
  val s = "aa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")  
  val result=matcher(br2, s)
  println(result)
}

//(ONE | "a") | (("a" | "aa") ~ (ONE | "a"))
@main
def test10() = {
  println("=====Test====")
  val br2=((ONE | (ZERO | "b")) | (((("a" ~ ZERO) | ("b" | "c")) | ((ONE | "a") ~ (ONE | "a")))))

  val s = "b"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")  
  val result=matcher(br2, s)
  println(result)
}

@main
def test11() = {
  println("=====Test====")
  val br2= (ONE|"a") ~  ( ("aa"| "a")    ~ ("a" | ONE))

  val s = "aaa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")  
  val result=matcher(br2, s)
  println(result)
}

@main
def strongTestNoSTARParallel() = {
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
  
  val numRegexes = BigInt(10_000_000_000L)
  val batchSize = BigInt(100_000L) 
  
  val batches = (BigInt(0) to numRegexes by batchSize).toVector.par
  batches.foreach { start =>
    val end = (start + batchSize - 1).min(numRegexes)
    for (i <- start to end) {
      val r = enumerate.decode(i)
      if (i % 100_000 == 0) { print("*") }
      for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "") {
          val markedBits=lex(r, s).getOrElse(Nil)
          val derBits= rebit.lex(r, s.toList)
          if (markedBits != derBits) {
            println(s"[${i}]-\n reg: $r \nstr: $s")
            println(s"\n${pp(r)}")
            println(s"markedBits= $markedBits\nderBits=$derBits")
            print("Type 'N' to exit, anything else to continue: ")
            val input = scala.io.StdIn.readLine()
            if (input.trim.toLowerCase == "n") {
                System.exit(1)
            }//end of if n -> exit
        }//end of if markedMatcher != derMatcher
      }// end of for s <- regenerate.generate_up_to(alphabet)(10)(r)
    }//end of for i <- start to end
  }// end of batches.foreach
  println("\nAll tests passed!")
}// end of strongTestNoSTARParallel

// End of SEQ case Tests