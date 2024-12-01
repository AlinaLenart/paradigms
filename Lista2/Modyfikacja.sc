/*Napisać funkcję inversionAB przyjmującą listę wartości oraz indeksy a i b, która zamienia
  kolejność elementów listy między indeksami a i b:
  [1; 2; 3; 4; 5; 6; 7; 8; 9] 𝑎=2 𝑏=4 → [1; 2; 5; 4; 3; 6; 7; 8; 9]
Funkcję napisać z zastosowaniem mechanizmu dopasowania wzorca oraz wybrać adekwatny
do zadania rodzaj rekurencji (zwykła lub ogonowa). (Scala)*/
import scala.annotation.tailrec

def inversionAB[A](list: List[A], a: Int, b: Int): List[A] =

  @tailrec
  def length(lst: List[A], count: Int = 0): Int = lst match {
    case Nil => count
    case _ :: t => length(t, count + 1)
  }

  def reverseList(lst: List[A]): List[A] =
    @tailrec
    def reverse_helper(source: List[A], reversed: List[A]): List[A] =
      source match {
        case Nil => reversed
        case h :: t => reverse_helper(t, h :: reversed)
      }
    reverse_helper(lst, Nil)


  def splitAtIndexes(lst: List[A], start: Int, end: Int): (List[A], List[A], List[A]) =
    @tailrec
    def split_loop(currList: List[A], currentIndex: Int,
             before: List[A], middle: List[A], after: List[A]): (List[A], List[A], List[A]) =
      currList match {
        case Nil => (reverseList(before), reverseList(middle), after)
        case h :: t =>
          if (currentIndex < start) split_loop(t, currentIndex + 1, h :: before, middle, after)
          else if (currentIndex <= end) split_loop(t, currentIndex + 1, before, h :: middle, after)
          else split_loop(t, currentIndex + 1, before, middle, h :: after)
      }
    split_loop(lst, 0, Nil, Nil, Nil)

  val len = length(list)
  val (minIndex, maxIndex) = 
    if (a > b) (b, a) 
    else (a, b)
  if (minIndex < 0 || maxIndex >= len)
    list //min to mniejszy, max wiekszy wiec wystarczy
  else
    val (before, middle, after) = splitAtIndexes(list, minIndex, maxIndex)
    before ::: reverseList(middle) ::: after


val list = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
inversionAB(list, 2, 4)
inversionAB(list, 4, 2)
inversionAB(list, 0, 8) //odwraca cala
inversionAB(list, 2, -1)