package funsets


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  private def max(a:Boolean, b:Boolean): Boolean =  a||b
  private def min (a:Boolean, b:Boolean): Boolean =  a&&b
  private def neg (a:Boolean): Boolean =  !a
  private def neg (a:Int => Boolean): Int => Boolean =  x => !a(x)

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
    def singletonSet(elem: Int): Set = x => if (x==elem) true else false
  

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
    def union(s: Set, t: Set): Set =  x => max(s(x),t(x))
  
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
    def intersect(s: Set, t: Set): Set = x => min(s(x),t(x))
  
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
    def diff(s: Set, t: Set): Set = x => if(s(x) && !t(x)) true else false
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
    def filter(s: Set, p: Int => Boolean): Set = x => s(x)&&p(x)
  

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
    def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (s(a)&& !p(a)) false
      //else if (s(a) != p(a)) false
      else iter(a+1)
    }
    iter(- bound)
  }
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
    def exists(s: Set, p: Int => Boolean): Boolean = neg(forall(s,neg(p)))
//    def iter(a: Int): Boolean = {
//      if (a > bound)
//      //else if (s(a)&& !p(a)) false
//      else if (s(a) != p(a)) false
//      else iter(a+1)
//    }
//    iter(0)

  
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
    def map(s: Set, f: Int => Int): Set = y => {
      var result:Boolean = false
      for(i <- -bound to bound )
      {
       result = result ||  contains(s,i) && y==f(i)
      }
      result
    } //x => s(f(x))
  
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }

  //
}
