//| scalaVersion: 3.8.4
//| mvnDeps: 
//|   - org.scala-lang:scala3-library_3:3.8.4

//====================================
// Call with
// 
// $ mill Generators.scala:run
//====================================

// Generators in Scala 3 — a translation of Oleg Kiselyov's generators.ml
// (https://okmij.org/ftp/continuations/generators.html)
//
// "Generators: the API for traversal, iteration and non-determinism."
//
// The OCaml original distinguishes two flavours of generator:
//
//   * PUSH generators - active: they drive the traversal themselves and
//     push each element to a user-supplied `visitor` function.
//        OCaml:  type 'a push_gen = ('a -> unit) -> unit
//
//   * PULL generators - passive: they hand out the next element only when
//     asked (`read`), threading a `handle' that encodes the traversal state.
//        OCaml:  a first-class module with an existential handle type `h`.
//
// Scala notes:
//
//   * Below only PULL generators are implemented for enumerations
//   * OCaml's `for_` / `let-` binding-operator sugar for push generators is
//     exactly Scala's for-comprehension: giving PushGen `flatMap`/`map`/
//     `withFilter`/`foreach` lets `for i <- g ...` and `yield` work directly.
//   * OCaml's existential first-class module (pull generator) becomes a trait
//     with an abstract type member `H` plus path-dependent types.
//   * OCaml's non-local exit via a local exception (`first_opt`, `with_break`)
//     becomes Scala 3's `scala.util.boundary` / `break`.

import scala.util.boundary
import scala.util.boundary.break


// ===========================================================================
// PUSH GENERATORS
//   OCaml:  type 'a push_gen = ('a -> unit) -> unit
// ===========================================================================

/** A push generator drives a traversal, invoking `visitor` on each element.
  *
  * Methods `foreach`/`map`/`flatMap`/`filter` make it usable in Scala
  * for-comprehensions — the analogue of Oleg's `for_`/`let-`/`yield` sugar.
  */
class PushGen[A](val run: (A => Unit) => Unit) {

  /** Run the generator, pushing each element to `visitor`. */
  def apply(visitor: A => Unit): Unit = run(visitor)
  def foreach(visitor: A => Unit): Unit = run(visitor)

  // --- combinators (all constant-space; fusion is automatic) ---

  /** OCaml: let map f g = fun visitor -> g (f >> visitor) */
  def map[B](f: A => B): PushGen[B] =
    PushGen(visitor => run(f andThen visitor))

  /** OCaml: let filter pred g = fun visitor -> g (fun i -> if pred i then visitor i) */
  def withFilter(pred: A => Boolean): PushGen[A] =
    PushGen(visitor => run(i => if pred(i) then visitor(i)))
 
  def filter(pred: A => Boolean): PushGen[A] = withFilter(pred)
  
  /** Nesting / flattening — the engine behind `cartesian` and `for`-yield. */
  def flatMap[B](f: A => PushGen[B]): PushGen[B] =
    PushGen(visitor => run(i => f(i).run(visitor)))

  /** OCaml: let append g1 g2 = fun visitor -> g1 visitor; g2 visitor */
  def ++(that: PushGen[A]): PushGen[A] =
    PushGen(visitor => { run(visitor); that.run(visitor) })

  // --- consumers ---

  /** OCaml: let to_list g = let l = ref [] in g (fun i -> l := i :: !l); List.rev !l */
  def toList: List[A] = {
    val buf = scala.collection.mutable.ListBuffer.empty[A]
    run { x => buf += x; () }
    buf.toList
  }

  /** First element, if any. Early exit via a non-local jump (OCaml: local exn). */
  def firstOpt: Option[A] = {
    boundary:
      run(i => break(Some(i)))
      None
  }

  /** OCaml: take n — restrict the traversal to the first `n` elements. */
  def take(n: Int): PushGen[A] = {
    PushGen { visitor =>
      boundary {
        var count = 0
        run { i =>
          if count >= n then break(())
          visitor(i)
          count += 1
        }
      }
    }
  }
}

// Companion object for PushGen

object PushGen {
  def apply[A](run: (A => Unit) => Unit): PushGen[A] = new PushGen[A](run)

  /** OCaml: let upto lo hi = fun visitor -> for i = lo to hi do visitor i done */
  def upto(lo: Int, hi: Int): PushGen[Int] =
    PushGen(visitor => for i <- lo to hi do visitor(i))

  /** OCaml: let cartesian g1 g2 = fun visitor -> g1 (fun i -> g2 (fun j -> visitor (i,j))) */
  def cartesian[A, B](g1: PushGen[A], g2: PushGen[B]): PushGen[(A, B)] =
    for i <- g1; j <- g2 yield (i, j)

  /** Do the two collections share an element? (nested-loop join). */
  def common[A](g1: PushGen[A], g2: PushGen[A]): Option[A] =
    cartesian(g1, g2).filter((x, y) => x == y).map(_._1).firstOpt

  /** All elements in common - the `yield` example, as a for-comprehension. */
  def commons[A](g1: PushGen[A], g2: PushGen[A]): PushGen[A] =
    for i <- g1; j <- g2 if i == j yield i
}
  


// ===========================================================================
// Demonstration / tests — mirroring the examples from the article.
// ===========================================================================

@main 
def demo(): Unit = {
  
  import PushGen.*
  
  // enumeration of regular expressions
  enum Rexp {
    case ZERO
    case ONE
    case CHAR(c: Char)
    case ALT(r1: Rexp, r2: Rexp)
    case SEQ(r1: Rexp, r2: Rexp)
    case STAR(r: Rexp)
  } 

  import Rexp.*

  def fromSeq[A](xs: Seq[A]): PushGen[A] = PushGen(v => xs.foreach(v)) 

  // enumerating Rexps using by default "ab" as alphabet
  def rexps(fuel: Int, alphabet: Seq[Char] = "ab".toSeq): PushGen[Rexp] =
    if fuel <= 0 then PushGen[Rexp](_ => ())            // nothing fits in depth 0
    else
      val sub  = rexps(fuel - 1, alphabet)              // built once, reused below
      val zero = PushGen[Rexp](v => v(ZERO))       // nullary leaves
      val one  = PushGen[Rexp](v => v(ONE))
      val chr  = fromSeq(alphabet).map(CHAR(_))    // leaf carrying a Char
      val alt  = for r1 <- sub; r2 <- sub yield ALT(r1, r2)   // binary
      val seq  = for r1 <- sub; r2 <- sub yield SEQ(r1, r2)   // binary
      val star = sub.map(STAR(_))                  // unary
      zero ++ one ++ chr ++ alt ++ seq ++ star


  // enumerates all regexes up to size 3 and 
  // takes the first 20_000 elements
  rexps(3).take(20_000).toList.foreach(println)
  // or: rexps(3).filter(myProperty).firstOpt  
      
}
      