//// Copyright Tony Morris 2008-2009
//// This software is released under an open source BSD licence.
//
//// $LastChangedRevision: 827 $
//// $LastChangedDate: 2009-09-02 20:42:27 +1000 (Wed, 02 Sep 2009) $
//
//
//package scalaz.memo
//
///**
// * Memoisation within a given range or size.
// *
// * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
// * @version $LastChangedRevision: 827 $<br>
// *          $LastChangedDate: 2009-09-02 20:42:27 +1000 (Wed, 02 Sep 2009) $<br>
// *          $LastChangedBy: tomjadams $
// */
//trait SizedMemo[K] {
//  /**
//   * Given the size, return a memoisation table of that given size.
//   *
//   * @param n The size of the memoisation table to create
//   * @return The memoisation table of the given size.
//   */
//  def apply[V](n: Int): Memo[K, V] = m[V](n)
//
//  /**
//   * Given the size, return a memoisation table of that given size.
//   *
//   * @param n The size of the memoisation table to create
//   * @return The memoisation table of the given size.
//   */
//  def m[V](n: Int): Memo[K, V]
//}
//
///**
// * Functions creating sized memoisation types.
// *
// * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
// * @version $LastChangedRevision: 827 $<br>
// *          $LastChangedDate: 2009-09-02 20:42:27 +1000 (Wed, 02 Sep 2009) $<br>
// *          $LastChangedBy: tomjadams $
// */
//object SizedMemo {
//  /**
//   * A sized memoisation technique. The size is ignored. No attempt to store computations is made.
//   *
//   * @return a sized memoisation technique.
//   */
//  def nilSizedMemo[K] = new SizedMemo[K] {
//    override def m[V](n: Int) = Memo.nilMemo
//  }
//
//  /**
//   * A sized memoisation technique. The size is used to create the backing array. Attempts to index outside the given
//   * size range are undefined. The <code>Memo</code> implementation that is returned contains a partial function.
//   *
//   * @return a sized memoisation technique.
//   */
//  def arraySizedMemo = new SizedMemo[Int] {
//    override def m[V](n: Int) = Memo.arrayMemo(n)
//  }
//
//  /**
//   * A sized memoisation technique backed with a mutable <code>HashMap</code>. The size is ignored.
//   *
//   * @return a sized memoisation technique.
//   */
//  def mutableHashMapSizedMemo = new SizedMemo[Int] {
//    override def m[V](n: Int) = Memo.mutableHashMapMemo
//  }
//
//  /**
//   * A sized memoisation technique backed with an immutable <code>EmptyMap</code>. The size is ignored.
//   *
//   * @return a sized memoisation technique.
//   */
//  def immutableEmptyMapSizedMemo = new SizedMemo[Int] {
//    override def m[V](n: Int) = Memo.immutableEmptyMapMemo
//  }
//
//  /**
//   * A sized memoisation technique backed with an immutable <code>HashMap</code>. The size is ignored.
//   *
//   * @return a sized memoisation technique.
//   */
//  def immutableHashMapSizedMemo = new SizedMemo[Int] {
//    override def m[V](n: Int) = Memo.immutableHashMapMemo
//  }
//
//
//  /**
//   * A sized memoisation technique backed with an immutable <code>ListMap</code>. The size is ignored.
//   *
//   * @return a sized memoisation technique.
//   */
//  def immutableListMapSizedMemo = new SizedMemo[Int] {
//    override def m[V](n: Int) = Memo.immutableListMapMemo
//  }
//
//
//  /**
//   * A sized memoisation technique backed with an immutable <code>TreeMap</code>. The size is ignored.
//   *
//   * @return a sized memoisation technique.
//   */
//  def immutableTreeMapSizedMemo = new SizedMemo[Int] {
//    override def m[V](n: Int) = Memo.immutableTreeMapMemo
//  }
//
//
//  /**
//   * A sized memoisation technique backed with an immutable <code>UnbalancedTreeMap</code>. The size is ignored.
//   *
//   * @return a sized memoisation technique.
//   */
//  def immutableUnbalancedTreeMapSizedMemo = new SizedMemo[Int] {
//    override def m[V](n: Int) = Memo.immutableUnbalancedTreeMapMemo
//  }
//}
