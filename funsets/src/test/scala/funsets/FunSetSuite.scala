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
      assert(contains(s1, 1), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("intersect of two given sets") {
    new TestSets:
      val u1 = union(s1, s2)
      val u2 = union(s2, s3)
      val s = intersect(u1, u2)
      assert(contains(s, 2))
      assert(!contains(s, 1))
      assert(!contains(s, 3))
  }

  test("diff returns only elements of s that are not in t") {
    new TestSets:
      val u1 = union(s1, s2)
      val u2 = union(s2, s3)
      val s = diff(u1, u2)
      assert(contains(s, 1))
      assert(!contains(s, 2))
      assert(!contains(s, 3))
  }

  test("filter all values below 3") {
    val s = (x: Int) => x >= 1 && x <= 5

    val f = filter(s, x => x >= 3)

    assert(!contains(f, 1))
    assert(!contains(f, 2))
    assert(contains(f, 3))
    assert(contains(f, 4))
    assert(contains(f, 5))
  }

  test("all numbers are bigger than 50") {
    val s = (x: Int) => x > 0 && x < 100
    val p = (x: Int) => x > 50
    val b = forall(s, p)

    assert(!b)
  }

  test("all numbers are positive") {
    val s = (x: Int) => x > 0
    val p = (x: Int) => x > 0

    assert(forall(s, p))
  }

  test("1 exists") {
    val s = (x: Int) => x > 0 && x <= 2
      assert(exists(s, x => x == 1))
  }

  test("map should double numbers") {
    val s = (x:Int) => x >= 1 && x <= 3
    val r = map(s, x => x * x)

    assert(contains(r, 1))
    assert(contains(r, 4))
    assert(contains(r, 9))
    assert(!contains(r, 2))
    assert(!contains(r, 3))
    assert(!contains(r, 20))
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
