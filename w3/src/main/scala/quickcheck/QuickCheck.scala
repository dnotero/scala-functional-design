package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("if insert only one element, findMin returns that element") = forAll { a: Int =>
    findMin(insert(a, empty)) == a
  }

  property("if insert two elements, findMin returns the minimun of the two") = forAll { (a: Int, b: Int) =>
    findMin(insert(b, insert(a, empty))) == min(a, b)
  }

  property("if insert one element and then delete it returns empty heap") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("if traverse heap, get elements in order") = forAll { h: H =>
    val list = toList(h)
    list == list.sorted
  }

  property("if meld two heaps, findMin on melded heap returns min of the two heaps") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
  }

  property("meld is commutative") = forAll { (h1: H, h2: H) =>
    toList(meld(h1, h2)) == toList(meld(h2, h1))
  }

  property("deleteMin delete min value, despite of the rank") = forAll{ h: H =>
    h == deleteMin(insert(findMin(h), h))
  }

  private def toList(h: H): List[A] = {
    @tailrec
    def toListAcc(list: List[A], h: H): List[A] = {
      if(isEmpty(h)) list
      else toListAcc(list :+ findMin(h) , deleteMin(h))
    }

    toListAcc(List(), h)
  }

  private def min(x: A, y: A) = if(x < y) x else y

}
