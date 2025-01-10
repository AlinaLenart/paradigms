import scala.annotation.tailrec

//sealed trait LList[+A]
//case object LNil extends LList[Nothing]
//case class LCons[A](head: A, tail: () => LList[A]) extends LList[A]


@tailrec
def skipElements[A](list: LazyList[A], step: Int): LazyList[A] = {
  if (step <= 0) then list
  else list match {
    case LazyList() => LazyList()
    case _ #:: tail => skipElements(tail, step- 1) //pomijanie rekurencyjne
  }
}

def skipTakeL[A](list: LazyList[A]): LazyList[A] = {
  def helper(remaining: LazyList[A], step: Int): LazyList[A] =
    remaining match {
      case LazyList() => LazyList()
      case head #:: tail =>
        head #:: helper(skipElements(tail, step), step + 1)
  }

  helper(list, 1)
}

def to_list[A](llist: LazyList[A]): List[A] = llist match {
  case LazyList() => List()
  case head #:: tail => head :: to_list(tail)
}

val lazy_list = LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
val result = skipTakeL(lazy_list)
val result_list = to_list(result)

val empty_list = LazyList()
val result2 = skipTakeL(empty_list)
val result2_list= to_list(result2)

val infinite_lazy_list: LazyList[Int] = LazyList.from(1)
//val result_infinite = skipTakeL(infinite_lazy_list)
//val result_list_infinite = to_list(result_infinite)

def limit[A](lazyList: LazyList[A], n: Int): LazyList[A] = {
  lazyList.take(n)
}
val limited = limit(infinite_lazy_list, 50)
val result_infinite = skipTakeL(limited)
val resultListInfinite = to_list(result_infinite)




