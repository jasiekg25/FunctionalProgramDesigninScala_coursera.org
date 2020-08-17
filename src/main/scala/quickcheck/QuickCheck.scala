package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =  for {
    n <- arbitrary[A]
    h <- frequency((1, const(empty)), (9, genHeap))
  }
    yield insert(n, h)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min of heap with two inserted values; hint1") = forAll { (h: H, elem0: Int, elem1: Int) =>
    isEmpty(h) match {
      case true => {
        val newHeap = insert(elem1, insert(elem0, h))
        findMin(newHeap) == Math.min(elem0, elem1)
      }
      case false => true
    }
  }
  property("delete min from one-element heap; hint2") = forAll { (elem: Int) =>
    deleteMin(insert(elem, empty)) == empty
  }

  property("sorted seq after continually finding and deleting min, hint3") = forAll { (h: H) =>
    def elemSeq(h: H, s: Seq[A]): Seq[A] = {
      if (isEmpty(h)) s
      else elemSeq(deleteMin(h), s ++ Seq(findMin(h)))
    }

    val seq = elemSeq(h, Seq())
    seq == seq.sorted
  }

  property("find min of two melded heaps, hint4") = forAll { (h0: H, h1: H) =>
    isEmpty(h0) match {
      case false => isEmpty(h1) match {
        case false => findMin(meld(h0, h1)) == Math.min(findMin(h0), findMin(h1)) // h0 non-empty, h1 non-empty
        case true => findMin(meld(h0, h1)) == findMin(h0) // h0 non-empty, h1 empty
      }
      case true => isEmpty(h1) match {
        case false => findMin(meld(h0, h1)) == findMin(h1) // h0 empty, h1 non-empty
        case true => true // h0 empty, h1 empty
      }
    }
  }

  property("meld empty results same heap") =
    forAll { (h: H) =>
      meld(h, empty) == h
    }

  property("meld empty results same heap") =
    forAll { (h: H) =>
      meld(empty, h) == h
    }
  property("meld") = forAll {
    (h1: H, h2: H) =>
      def heapEqual(h1: H, h2: H): Boolean =
        if (isEmpty(h1) && isEmpty(h2)) true
        else findMin(h1) == findMin(h2) && heapEqual(deleteMin(h1), deleteMin(h2))
      heapEqual(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

}
