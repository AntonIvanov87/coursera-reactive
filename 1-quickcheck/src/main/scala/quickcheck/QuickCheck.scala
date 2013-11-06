package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("minNonEmpty") = forAll {numAndHeap: (Int, H) =>
    val heapWithNum = insert(numAndHeap._1, numAndHeap._2)
    findMin(heapWithNum) == Math.min(numAndHeap._1, findMin(numAndHeap._2))
  }

  property("deleteMin1") = forAll {num: Int =>
    val heapWithNum = insert(num, empty)
    isEmpty(deleteMin(heapWithNum))
  }

  property("deleteMin2") = forAll {numAndNum: (Int, Int) =>
    val heapWith2Nums = insert(numAndNum._1, insert(numAndNum._2, empty))
    val heapWith1Num = deleteMin(heapWith2Nums)
    findMin(heapWith1Num) == Math.max(numAndNum._1, numAndNum._2)
  }

  property("deleteMinNonEmpty") = forAll {heap: H =>
    val mins = minsAcc(heap, List.empty)
    mins.sorted == mins
  }
  
  property("deleteMinAfterMeld") = forAll {twoHeaps: (H, H) =>
    val bigHeap = meld(twoHeaps._1, twoHeaps._2)
    val mins = minsAcc(bigHeap, List.empty)
    mins.sorted == mins && mins.contains(findMin(twoHeaps._1)) && mins.contains(findMin(twoHeaps._2))
  }

  @tailrec
  final def minsAcc(heap: H, acc: List[Int]): List[Int] = {
    if (isEmpty(heap)) {
      acc.reverse
    } else {
      minsAcc(deleteMin(heap), findMin(heap) :: acc)
    }
  }

  property("insertBack") = forAll {heap: H =>
    val minOfHeap = findMin(heap)
    val smallerHeap = deleteMin(heap)
    val reconstructedHeap = insert(minOfHeap, smallerHeap)
    findMin(reconstructedHeap) == minOfHeap
  }

  property("meldNonEmptyAndEmpty") = forAll {heap: H =>
    val heapAndEmpty = meld(heap, empty)
    findMin(heapAndEmpty) == findMin(heap)
  }

  property("meldEmptyAndNonEmpty") = forAll {heap: H =>
    val emptyAndHeap = meld(empty, heap)
    findMin(emptyAndHeap) == findMin(heap)
  }

  property("meld2Heaps") = forAll {heapAndHeap: (H, H) =>
    val meldHeap = meld(heapAndHeap._1, heapAndHeap._2)
    findMin(meldHeap) == Math.min(findMin(heapAndHeap._1), findMin(heapAndHeap._2))
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    subHeap <- oneOf(value(empty), genHeap)
  } yield insert(x, subHeap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
