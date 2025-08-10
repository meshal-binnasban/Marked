//
// Generating strings for regular expressions
//
// Haskell code by Thiemann et al
// Scala translation by David Wang
//
// Call with
//
//  amm regenerate.sc test
//  amm regenerate.sc problem

import $file.rexp, rexp._

// generic operations on LazyLists
import math.Ordering.Implicits.infixOrderingOps

def merge[A](xs: LazyList[A], ys: LazyList[A])(using Ordering[A]): LazyList[A] = (xs, ys) match {
    case (LazyList(), l) => l
    case (l, LazyList()) => l
    case (x#::xs1, y#::ys1) => {
            if (x == y) x #:: merge(xs1, ys1)
            else if (x <= y) x #:: merge(xs1, ys)
            else y #:: merge(xs, ys1)
        }
}

def inter[A](xs: LazyList[A], ys: LazyList[A])(using Ordering[A]): LazyList[A] = (xs, ys) match {
    case (LazyList(), _) => LazyList()
    case (_, LazyList()) => LazyList()
    case (x#::xs1, y#::ys1) => {
        if (x == y) x #:: inter(xs1, ys1)
        else if (x < y) inter(xs1, ys)
        else inter(xs, ys1)
    }
}
def diff[A](xs: LazyList[A], ys: LazyList[A])(using Ordering[A]): LazyList[A] = (xs, ys) match {
    case (LazyList(), _) => LazyList()
    case (xss, LazyList()) => xss     // Saskia found a problem in the original code
    case (x#::xs1, y#::ys1) => {
        if (x == y) diff(xs1, ys1)
        else if (x < y) x #:: diff(xs1, ys)
        else diff(xs, ys1)
    }
}


abstract class Lang
case object Null extends Lang
case class Data(lot: LazyList[String]) extends Lang // some language of strings of a length
case class Univ(lot: LazyList[String]) extends Lang // the full language of strings of length

def mkData(strs: LazyList[String]): Lang = strs match {
    case LazyList() => Null
    case xs         => Data(xs)
}

def union1(xs: Lang, ys: Lang): Lang = (xs, ys) match {
    case (Null, yss)            => yss
    case (xss, Null)            => xss
    case (Univ(_), _)           => xs
    case (_, Univ(_))           => ys
    case (Data(xss), Data(yss)) => mkData(merge(xss, yss))
}

def inter1(xs: Lang, ys: Lang): Lang = (xs, ys) match {
    case (_, Null)              => Null
    case (Null, _)              => Null
    case (xss, Univ(_))         => xss
    case (Univ(_), yss)         => yss
    case (Data(xss), Data(yss)) => mkData(inter(xss, yss))
}

def diff1(xs: Lang, ys: Lang): Lang = (xs, ys) match {
    case (xs, Null)             => xs
    case (Null, ys)             => Null
    case (_, Univ(_))           => Null
    case (Univ(xss), Data(yss)) => mkData(diff(xss, yss))
    case (Data(xss), Data(yss)) => mkData(diff(xss, yss))
}

def concatLang(xs: Lang, ys: Lang): Lang = (xs, ys) match {
    case (_, Null)              => Null
    case (Null, _)              => Null
    case (Univ(xs1), Univ(ys1)) => Univ(xs1.flatMap(x => ys1.map(y => x ++ y)))
    case (Data(xls), Data(yls)) => Data(xls.flatMap(x => yls.map(y => x ++ y)))
    case (Data(xls), Univ(yls)) => Data(xls.flatMap(x => yls.map(y => x ++ y))) 
    case (Univ(xls), Data(yls)) => Data(xls.flatMap(x => yls.map(y => x ++ y))) 
}

def flattenLang(xs: Lang): LazyList[String] = xs match {
    case Null => LazyList()
    case Data(xss) => xss
    case Univ(xss) => xss
}

abstract class SegType
// the language of strings of a certain length
case class Cons(lang: Lang) extends SegType 

// the language of all strings surpassing a certain length -- can only occur as the final element in a lazylist
case class Full(all: LazyList[Lang]) extends SegType 

type Segments = LazyList[SegType]

def unionSegs(xs: Segments)(ys: Segments): Segments = (xs, ys) match {
    case (LazyList(), segs)                     => segs
    case (segs, LazyList())                     => segs
    case (Full(xls)#::xs1, _)                   => Full(xls)#::xs1
    case (_, Full(yls)#::ys1)                   => Full(yls)#::ys1
    case (Cons(xl)#::xsegs, Cons(yl)#::ysegs)   => Cons(union1(xl, yl))#::unionSegs(xsegs)(ysegs)
}

def interSegs(xs: Segments)(ys: Segments): Segments = (xs, ys) match {
    case (LazyList(), _)                        => LazyList()
    case (_, LazyList())                        => LazyList()
    case (Full(_)#::_, segs)                    => segs
    case (segs, Full(_)#::_)                    => segs
    case (Cons(xs)#::xsegs, Cons(ys)#::ysegs)   => Cons(inter1(xs, ys))#::interSegs(xsegs)(ysegs)
}

def diffSegs(xs: Segments)(ys: Segments): Segments = (xs, ys) match {
    case (segs, LazyList())                         => segs
    case (LazyList(), _)                            => LazyList()
    case (segs, Full(_)#::_)                        => segs
    case (Full(xl#::xls)#::xs1, Cons(yls)#::ysegs)  =>
        Cons(diff1(xl, yls))#::diffSegs(Full(xls)#::xs1)(ysegs)
}

def sigmaStarSegs(sigma: LazyList[Char]): Segments = {
    def segments: LazyList[LazyList[String]] = LazyList("") #:: segments.map(extend)
    def extend(segment: LazyList[String]): LazyList[String] = sigma.flatMap(x => segment.map(x.toString ++ _))
    LazyList(Full(segments.map(Univ(_))))
}

def complementSegs(sigma: LazyList[Char])(xs: Segments): Segments = 
    diffSegs(sigmaStarSegs(sigma))(xs)

def flattenSegs(xs: Segments): LazyList[String] = xs match {
    case LazyList()         => LazyList()
    case Cons(lang)#::segs  => flattenLang(lang) #::: flattenSegs(segs)
    case Full(langs)#::segs => langs.flatMap(flattenLang)
}

def flattenSegs1(xs: Segments): LazyList[LazyList[String]] = xs match {
    case LazyList()         => LazyList()
    case Cons(lang)#::segs  => flattenLang(lang)#::flattenSegs1(segs)
    case Full(langs)#::segs => langs.map(flattenLang)
}

def updateMax(a: Int, segs: Segments, b: Option[Int], c: Option[Int])
: (Lang, Segments, Option[Int], Option[Int]) = ((a, segs, b, c): @unchecked) match {
    case (_, _, Some(_), _)                         => (Null, LazyList(), b, c)
    case (_, LazyList(), None, _)                   => (Null, LazyList(), Some(a), c)
    case (_, Cons(xs)#::xss, None, None)            => (xs, xss, None, None)
    case (_, Full(xs#::xss)#::xss1, None, None)     => (xs, Full(xss)#::xss1, None, Some(a))
    case (_, Full(xs#::xss)#::xss1, None, Some(_))  => (xs, Full(xss)#::xss1, None, c)
}

def sigmaStarFromNth(sigma: LazyList[Char], n: Int): Segments = {
    def fromNthFull(xs: Segments): Segments = xs match {
        case Full(xss)#::xss1 => Full(xss.drop(n))#::xss1
    }
    fromNthFull(sigmaStarSegs(sigma))
}

def concatenate(sigma: LazyList[Char], xsegs0: Segments, ysegs0: Segments): Segments = {
    def collect(xsegs: Segments, ysegs: Segments, mmx: Option[Int], mmy: Option[Int], 
            mfx: Option[Int], mfy: Option[Int], ryss: LazyList[Lang], n: Int): Segments = {
        val (xln, xsegs1, mmx1, mfx1) = updateMax(n, xsegs, mmx, mfx)
        val (yln, ysegs1, mmy1, mfy1) = updateMax(n, ysegs, mmy, mfy)
        val mBound: Option[Int] = (mmx, mmy) match {
                case (Some(x), Some(y)) => Some(x + y)
                case _                  => None
            }
        val mFullBound: Option[Int] = (mfx, mfy) match {
                case (Some(x), Some(y)) => Some(x + y)
                case _                  => None
            }
        def ryss1: LazyList[Lang] = yln #:: ryss
        (mBound, mFullBound) match {
            case (Some(m), _) if (n >= m - 1)   => LazyList()
            case (_, Some(f)) if (n >= f)       => sigmaStarFromNth(sigma, n)
            case _                              => {
                Cons(concatWithSegments(xsegs0, ryss1).foldRight[Lang](Null)(union1)) #:: ((mmy1) match { 
                    case None       => collect(xsegs1, ysegs1, mmx1, mmy1, mfx1, mfy1, ryss1, n + 1)
                    case Some(_)    => collect1(xsegs1, ysegs1, mmx1, mmy1, mfx1, mfy1, segsToList(xsegs0).take(n + 1).reverse, n + 1)
                })
            }
        }
    }

    def collect1(xsegs: Segments, ysegs: Segments, mmx: Option[Int], mmy: Option[Int], 
            mfx: Option[Int], mfy: Option[Int], rxss: LazyList[Lang], n: Int): Segments = {
        val (xln, xsegs1, mmx1, mfx1) = updateMax(n, xsegs, mmx, mfx)
        val (yln, ysegs1, mmy1, mfy1) = updateMax(n, ysegs, mmy, mfy)
        val mBound: Option[Int] = (mmx, mmy) match {
                case (Some(x), Some(y)) => Some(x + y)
                case _                  => None
            }
        val mFullBound: Option[Int] = (mfx, mfy) match {
                case (Some(x), Some(y)) => Some(x + y)
                case _                  => None
            }
        def rxss1: LazyList[Lang] = xln #:: rxss
        (mBound, mFullBound) match {
            case (Some(m), _) if (n >= m - 1)   => LazyList()
            case (_, Some(f)) if (n >= f)       => sigmaStarFromNth(sigma, n)
            case _                              => {
                Cons(concatWithSegments1(rxss1, ysegs0)
                    .foldRight[Lang](Null)(union1)) #:: 
                    collect1(xsegs1, ysegs1, mmx1, mmy1, mfx1, mfy1, rxss1, n + 1)
            }
        }
    }
    collect(xsegs0, ysegs0, None, None, None, None, LazyList(), 0)
}

def concatWithSegments(xs: Segments, ys: LazyList[Lang]): LazyList[Lang] = (xs, ys) match {
    case (LazyList(), _)                    => LazyList()
    case (_, LazyList())                    => LazyList()
    case (Cons(xl)#::xsegs, yl#::yss)       => concatLang(xl, yl) #:: concatWithSegments(xsegs, yss)
    case (Full(xl#::xss)#::_, yl#::yss)     => concatLang(xl, yl) #:: xss.zip(yss).map((concatLang(_, _)).tupled)
}

def concatWithSegments1(ys: LazyList[Lang], xs: Segments): LazyList[Lang] = (ys, xs) match {
    case (LazyList(), _)                    => LazyList()
    case (_, LazyList())                    => LazyList()
    case (yl#::yss, Cons(xl)#::xsegs)       => concatLang(yl, xl) #:: concatWithSegments1(yss, xsegs)
    case (yl#::yss, Full(xl#::xss)#::_)     => concatLang(yl, xl) #:: yss.zip(xss).map((concatLang(_, _)).tupled)
}

def segsToList(xs: Segments): LazyList[Lang] = xs match {
    case LazyList()         => LazyList()
    case Cons(xs)#::xsegs   => xs #:: segsToList(xsegs)
    case Full(xss)#::_      => xss
}

def star(xs: Segments): Segments = xs match {
    case LazyList()         => Cons(Univ(LazyList("")))#::LazyList()
    case Full(ls)#::xss     => Full(ls)#::xss
    case Cons(_)#::xsegs    => {
        def ysegs: Segments = Cons(Univ(LazyList(""))) #:: collect (ysegs, LazyList())
        def collect(lsegs: Segments, rsegs: LazyList[Lang]): Segments = lsegs match {
            case Cons(ysegi)#::ysegs1 => {
                val rsegs1: LazyList[Lang] = ysegi #:: rsegs
                Cons(concatWithSegments(xsegs, rsegs1)
                    .foldRight[Lang](Null)(union1)) #::
                    collect(ysegs1, rsegs1)
            }
            case _ => LazyList()
        }
        ysegs
    }
}

def generateSegs(sigma: LazyList[Char])(r: Rexp): Segments = {
    def gen(r: Rexp): Segments = r match {
        case ZERO           => LazyList()
        case ONE            => LazyList(Cons(Data(LazyList(""))))
        case CHAR(c)        => Cons(Null)#::(Cons(Data(LazyList(c.toString)))#::LazyList())
        case SEQ(r1, r2)    => concatenate(sigma, gen(r1), gen(r2))
        case ALT(r1, r2)    => unionSegs(gen(r1))(gen(r2))
        case NOT(r)         => complementSegs(sigma)(gen(r))
        case STAR(r)        => star(gen(r))
        case NTIMES(r, 0) => LazyList(Cons(Data(LazyList(""))))
        case NTIMES(r, n) => concatenate(sigma, gen(r), gen(NTIMES(r, n - 1)))
    }
    gen(r)
}

def generate(sigma: LazyList[Char])(r: Rexp): LazyList[String] =
    flattenSegs(generateSegs(sigma)(r))

def generate_up_to(sigma: LazyList[Char])(l: Int)(r: Rexp): LazyList[String] =
    flattenSegs(generateSegs(sigma)(r).take(l + 1)) // + 1 because the first segment contains strings of length 0




@main
def test() = {
      val alphabet = LazyList('a', 'b')
      val r = STAR(ALT(CHAR('a'), SEQ(CHAR('b'),CHAR('a'))))

      println(s"regular expression: $r")

      println("20 positive examples (strings that match)")
      generate_up_to(alphabet)(20)(r).take(20).foreach(println(_))

}


// generation-up-to an explicit size works, but not the
// `lazy' version:
