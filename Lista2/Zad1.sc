import scala.annotation.tailrec

def indexSwap[A](list: List[A], a: Int, b: Int): List[A] =

  @tailrec
  def length(list: List[A], count: Int): Int =
    list match
      case Nil => count
      case _ :: t => length(t, count + 1)


  @tailrec
  def findElements[A](list: List[A], count: Int, tempA: Option[A], tempB: Option[A]): (Option[A], Option[A]) =
    list match
      case Nil => (tempA, tempB)
      case h :: t =>
        (count, tempA, tempB) match
          case (c, _, _) if c == a => findElements(t, count + 1, Some(h), tempB)
          case (_, _, _) if count == b => findElements(t, count + 1, tempA, Some(h))
          case _ => findElements(t, count + 1, tempA, tempB)


  def replaceElements[A](list: List[A], count: Int, tempA: Option[A], tempB: Option[A]): List[A] =
    list match
      case Nil => Nil //musi byc jako pierwszy
      case h :: t =>
        (count, tempA, tempB) match
          case (c, _, Some(bValue)) if c == a => bValue :: replaceElements(t, count + 1, tempA, None)
          case (_, Some(aValue), _) if count == b => aValue :: replaceElements(t, count + 1, None, tempB)
          case _ => h :: replaceElements(t, count + 1, tempA, tempB)

  val len = length(list, 0)
  (a, b) match
    case (a, b) if a < 0 || b < 0 || a >= len || b >= len => list
    case _ =>
      val (tempA, tempB) = findElements(list, 0, None, None)
      replaceElements(list, 0, tempA, tempB)



val indexSwap13: List[Int] => List[Int] =
  anyList => indexSwap(anyList, 1, 3)

val list1 = List(1, 2, 3, 4, 5)
val list2 = List(6, 7, 8, 9, 10)
val listEmpty = List()
val listOutOfIndex = List(0, 1)

indexSwap13(list1)
indexSwap13(list2)
indexSwap13(listEmpty)
indexSwap13(listOutOfIndex)