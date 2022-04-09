package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s1to5 = (elem: Int) => (1 to 5).contains(elem)

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton 1")
  }

  test("singleton set 2 contains 2") {
    new TestSets: 
      assert(contains(s2, 2), "Singleton 2")
  }

  test("singleton set 3 not contains 2") {
    new TestSets: 
      assert(!contains(s3, 2), "Singleton 3")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("intersect contains elements in common") {
    new TestSets:
      val s = union(s1, s2)
      val t = union(s1, s3)
      val i = intersect(s, t)
      assert(contains(i, 1))
      assert(!contains(i, 2))
      assert(!contains(i, 3))
  }

  test("diff gives all of s that are not in s") {
    new TestSets:
      val s = union(s1, s2)
      val t = union(s1, s3)
      val d = diff(s, t)
      assert(contains(d, 2))
      assert(!contains(d, 1))
      assert(!contains(d, 3))
  }

  test("filter even only contains even integers") {
    new TestSets:
      val isEven = (elem: Int) => elem % 2 == 0
      val f = filter(s1to5, isEven)
      assert(!contains(f, 1))
      assert(contains(f, 2))
      assert(!contains(f, 3))
      assert(contains(f, 4))
      assert(!contains(f, 5))
  }

  test("forall smaller 6 is true") {
    new TestSets:
      val smaller6 = (elem: Int) => elem < 6
      assert(forall(s1to5, smaller6))
  }

  test("forall smaller 0 is false") {
    new TestSets:
      val smaller0 = (elem: Int) => elem < 0
      assert(!forall(s1to5, smaller0))
  }

  test("forall smaller 5 is false") {
    new TestSets:
      val smaller5 = (elem: Int) => elem < 5
      assert(!forall(s1to5, smaller5))
  }

  test("exists smaller 6 is true") {
    new TestSets:
      val smaller6 = (elem: Int) => elem < 6
      assert(exists(s1to5, smaller6))
  }

  test("exists smaller 0 is false") {
    new TestSets:
      val smaller0 = (elem: Int) => elem < 0
      assert(!exists(s1to5, smaller0))
  }

  test("exists smaller 5 is true") {
    new TestSets:
      val smaller5 = (elem: Int) => elem < 5
      assert(exists(s1to5, smaller5))
  }

  test("exists 4 is true") {
    new TestSets:
      val is4 = (elem: Int) => elem == 4
      assert(exists(s1to5, is4))
  }


  test("map x => x + 10") {
    new TestSets:
      val add10 = (x: Int) => (x + 10)
      val mapped = map(s1to5, add10)
      assert(contains(mapped, 11))
      assert(contains(mapped, 12))
      assert(contains(mapped, 13))
      assert(contains(mapped, 14))
      assert(contains(mapped, 15))
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
