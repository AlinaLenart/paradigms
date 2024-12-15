import scala.annotation.tailrec

sealed trait LList[+A]
case object LNil extends LList[Nothing]
case class LCons[A](head: A, tail: () => LList[A]) extends LList[A]


@tailrec
def skipElements[A](list: LList[A], step: Int): LList[A] = {
  if (step <= 0) then list
  else list match {
    case LNil => LNil
    case LCons(_, tail) => skipElements(tail(), step - 1) //pomijanie rekurencyjne
  }
}

def skipTakeL[A](list: LList[A]): LList[A] = {
  def loop(remaining: LList[A], step: Int): LList[A] =
    remaining match {
      case LNil => LNil
      case LCons(head, tail) =>
        LCons(head, () => loop(skipElements(tail(), step), step + 1))
  }

  loop(list, 1)
}

def to_list[A](llist: LList[A]): List[A] = llist match {
  case LNil => Nil
  case LCons(head, tail) => head :: to_list(tail())
}

val lazy_list: LList[Int] = LCons(1, () => LCons(2, () => LCons(3, () => LCons(4, () => LCons(5, () => LCons(6, () => LCons(7, () => LCons(8, () => LCons(9, () => LCons(10, () => LNil))))))))))
val result = skipTakeL(lazy_list)
val result_list = to_list(result)

val empty_list: LList[Int] = LNil
val result2 = skipTakeL(empty_list)
val result2_list= to_list(result2)

