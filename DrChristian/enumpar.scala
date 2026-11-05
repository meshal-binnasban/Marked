//| scalaVersion: 3.8.4
//| scalacOptions: ["-deprecation", "-feature"]
// construction data
type CDATA[T] = List[(Int, List[T] => T)]


/** Immutable, precomputed enumerator for T, valid for every tree of up to
  * `maxSize` nodes. Build one with `Enumeration.covering` / `Enumeration.upTo`.
  * Pure and shareable: safe to call `decode` from many threads at once. */
final class Enumeration[T] private (
    val cd: CDATA[T],
    val maxSize: Int,
    counts: Vector[BigInt],         // counts(n)   = #trees with exactly n nodes
    forest: Vector[Vector[BigInt]], // forest(m)(a)= #ordered a-tuples, total m nodes
    totals: Vector[BigInt]):        // totals(n)   = counts(1) + ... + counts(n)

  val arities: Vector[Int] = cd.map(_._1).toVector

  // Number of trees with exactly n nodes (generalised Catalan number).
  def count(n: Int): BigInt =
    if n <= 0 || n > maxSize then BigInt(0) else counts(n)

  // Total number of trees of size 1..n
  def total(n: Int): BigInt =
    if n <= 0 then BigInt(0)
    else if n >= maxSize then totals(maxSize) else totals(n)

  // Number of ordered tuples of trees whose node counts sum to m.
  private def countForest(a: Int, m: Int): BigInt = forest(m)(a)

  // ---- decode :  N => T  (unrank) -------------------------------------
  def decode(k: BigInt): T =
    val (n, rank) = findSize(k)
    unrankTree(rank, n)

  // smallest n with totals(n) > k, by binary search over the prefix sums
  def findSize(k: BigInt): (Int, BigInt) =
    @annotation.tailrec
    def search(lo: Int, hi: Int): Int =
      if lo >= hi then lo
      else
        val mid = (lo + hi) >>> 1
        if totals(mid) > k then search(lo, mid) else search(mid + 1, hi)
    val n = search(1, maxSize)
    (n, k - totals(n - 1))

  def unrankTree(rank: BigInt, n: Int): T =
    @annotation.tailrec
    def go(ci: Int, r: BigInt): T =
      if ci >= cd.length then throw new IllegalStateException("rank out of range")
      else
        val block = countForest(arities(ci), n - 1)
        if r < block then cd(ci)._2(unrankForest(r, arities(ci), n - 1))
        else go(ci + 1, r - block)
    go(0, rank)

  def unrankForest(rank: BigInt, a: Int, m: Int): List[T] =
    if a == 0 then Nil
    else
      @annotation.tailrec
      def go(s: Int, r: BigInt): List[T] =
        if s > m - (a - 1) then throw new IllegalStateException("forest rank out of range")
        else
          val cf    = countForest(a - 1, m - s)
          val block = count(s) * cf
          if r < block then
            unrankTree(r / cf, s) :: unrankForest(r % cf, a - 1, m - s)
          else go(s + 1, r - block)
      go(1, rank)

end Enumeration

object Enumeration:
  /** Build the tables with one bottom-up, purely functional fold. It threads
    * three immutable Vectors and appends one entry per size -- nothing is
    * mutated in place. Growth stops once `stop(totalSoFar)` holds or `limit`
    * sizes have been built. */
  def build[T](cd: CDATA[T], stop: BigInt => Boolean, limit: Int): Enumeration[T] =
    val arities  = cd.map(_._1).toVector
    val maxArity = arities.max

    @annotation.tailrec
    def loop(m: Int,
             counts: Vector[BigInt],
             forest: Vector[Vector[BigInt]],
             totals: Vector[BigInt]): Enumeration[T] =
      if m >= limit || stop(totals(m)) then
        new Enumeration(cd, m, counts, forest, totals)
      else
        // forest(m)(a) = #ordered a-tuples of trees with total m nodes
        val row = (0 to maxArity).toVector.map { a =>
          if a == 0 then (if m == 0 then BigInt(1) else BigInt(0))
          else (1 to (m - (a - 1)))
                 .foldLeft(BigInt(0))((acc, s) => acc + counts(s) * forest(m - s)(a - 1))
        }
        // count(m+1) sums the row over the constructor LIST (arities repeat)
        val cnt = arities.foldLeft(BigInt(0))((acc, a) => acc + row(a))
        loop(m + 1, counts :+ cnt, forest :+ row, totals :+ (totals(m) + cnt))

    loop(0, Vector(BigInt(0)), Vector.empty, Vector(BigInt(0)))

  /** Enumerator able to handle every tree of up to `maxSize` nodes. */
  def upTo[T](cd: CDATA[T], maxSize: Int): Enumeration[T] =
    build(cd, _ => false, maxSize)

  /** Enumerator able to decode every index in 0 .. maxIndex. */
  def covering[T](cd: CDATA[T], maxIndex: BigInt): Enumeration[T] =
    build(cd, total => total > maxIndex, Int.MaxValue)

// ---- top-level helpers (keep the `given` + `decode(i)` calling style) ---

def decode[T](k: BigInt)(using e: Enumeration[T]): T  = e.decode(k)
def total[T](n: Int)(using e: Enumeration[T]): BigInt = e.total(n)
def count[T](n: Int)(using e: Enumeration[T]): BigInt = e.count(n)


// =====================================================================
// Example 1 : regular expressions
// =====================================================================


/* 

given rexp_enum: Enumeration[Rexp] = Enumeration.covering(List(
  (0, _  => ONE),
  (0, _  => ZERO),
  (0, _  => CHAR('a')),
  (0, _  => CHAR('b')),
  (0, _  => CHAR('c')),
  (1, cs => STAR(cs(0))),
  (2, cs => ALT(cs(0), cs(1))),
  (2, cs => SEQ(cs(0), cs(1)))
), maxIndex = 500_000_000L)

 */