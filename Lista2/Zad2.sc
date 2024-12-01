import scala.annotation.tailrec;

def split3Rec[A](list: List[A]) : (List[A], List[A], List[A]) =
  list match {
    case x :: y :: z :: tail =>
      val (l1, l2, l3) = split3Rec(tail)
      (x :: l1, y :: l2, z :: l3)
    case _ => (Nil, Nil, Nil)
  }

def split3Tail[A](list: List[A]) : (List[A], List[A], List[A]) =
  @tailrec
  def split3Tail_helper(list: List[A], accum: (List[A], List[A], List[A])): (List[A], List[A], List[A]) =
    (list, accum) match {
      case (x :: y :: z :: tail, (l1, l2, l3)) => split3Tail_helper(tail, (x :: l1, y :: l2, z :: l3))
      case _ => accum
    }
  split3Tail_helper(list, (Nil, Nil, Nil))



val list1 = List(1, 2, 3, 4, 5, 6)
val list2 = List(6, 7, 8, 9, 10)
val listEmpty = List()
split3Rec(list1)
split3Rec(list2)
split3Rec(listEmpty)
split3Tail(list1)
split3Tail(list2)
split3Tail(listEmpty)























/*

def length[A](list: List[A]): Int =
  list match {
    case _ :: t => 1 + length(t)
    case Nil => 0
  }

def split3Tail[A](list: List[A]): (List[A], List[A], List[A]) = {

  @scala.annotation.tailrec
  def split3tail_aux(list: List[A], acc: (List[A], List[A], List[A])): (List[A], List[A], List[A]) =
    (list, acc) match {
      case (x :: y :: z :: t, (l1, l2, l3)) => split3tail_aux(t, (l1 :+ x, l2 :+ y, l3 :+ z)) //ns koniec
      case _ => acc
    }

  split3tail_aux(list, (Nil, Nil, Nil))
}

def split3Tail[A](list: List[A]): (List[A], List[A], List[A]) = {

  @scala.annotation.tailrec
  def split3tail_aux(list: List[A], acc: (List[A], List[A], List[A])): (List[A], List[A], List[A]) =
    (list, acc) match {
      case (x :: y :: z :: t, (l1, l2, l3)) => split3tail_aux(t, (x :: l1, y :: l2, z :: l3))
      case _ => acc
    }

  @scala.annotation.tailrec
  def manualReverse[A](lst: List[A], acc: List[A]): List[A] = lst match {
    case Nil => acc
    case h :: t => manualReverse(t, h :: acc)
  }

  val (l1, l2, l3) = split3tail_aux(list, (Nil, Nil, Nil))
  (manualReverse(l1, Nil), manualReverse(l2, Nil), manualReverse(l3, Nil))
}
*/