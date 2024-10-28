def split2Helper(list: List[Int], list_a: List[Int], list_b: List[Int], a: Int, b: Int): (List[Int], List[Int]) = {
    if (list == Nil) {
      (list_a, list_b)
    } else {
      val head = list.head
      val tail = list.tail
      val list_a_new =
        if (head <= a) head :: list_a
        else list_a
      val list_b_new =
        if (head >= b) head :: list_b
        else list_b
      split2Helper(tail, list_a_new, list_b_new, a, b)
    }
  }

def split2(list: List[Int], a: Int, b: Int): (List[Int], List[Int]) = {
  split2Helper(list, List(), List(), a, b)
}

val list1 = List(1, 2, 3, 4, 5, 6)
val a1 = 3
val b1 = 4
println(split2(list1, a1, b1))

val list2 = List(1, 2, 3, 4)
val a2 = 3
val b2 = 13
println(split2(list2, a2, b2))

val listEmpty = List()
val a3 = 0
val b3 = 10
println(split2(listEmpty, a3, b3))

val list3 = List(1, 2, 3, 4, 5, 6)
val a4 = 6
val b4 = 1
println(split2(list3, a4, b4))
