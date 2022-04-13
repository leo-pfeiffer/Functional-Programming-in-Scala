package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:

  lazy val genHeap: Gen[H] = 
    for
      a <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    yield insert(a, h)

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) => 
    val h = insert(a, insert(b, empty))
    findMin(h) == (a min b)
  }

  property("delete1") = forAll{ (a: Int) => 
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("sorted sequence") = forAll { (h: H) =>
    def iter(h: H, ls: List[Int]): List[Int] = {
      if isEmpty(h) then ls
      else findMin(h) :: iter(deleteMin(h), ls)
    }
    val ls = iter(h, Nil)
    ls == ls.sorted
  }

  property("min after melding") = forAll{ (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == (findMin(h1) min findMin(h2))
  }

  property("melded elements is set of original") = forAll{(h1: H, h2: H) =>
    def setFromHeap(h: H, st: Set[Int] = Set()): Set[Int] = {
      if isEmpty(h) then st
      else Set(findMin(h)) ++ setFromHeap(deleteMin(h), st)
    }
    val m = meld(h1, h2)
    setFromHeap(m) == (setFromHeap(h1) ++ setFromHeap(h2))
  }

