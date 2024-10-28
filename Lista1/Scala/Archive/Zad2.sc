import scala.annotation.tailrec

@tailrec
def hits_helper[Elem](list1: List[Elem], list2: List[Elem], count: Int): Int = { //Elem - typ ogolny
  if (list1.isEmpty || list2.isEmpty) count
  else {
    val head1 = list1.head
    val head2 = list2.head
    val tail1 = list1.tail
    val tail2 = list2.tail

    if (head1 == head2)
      hits_helper(tail1, tail2, count + 1)
    else
      hits_helper(tail1, tail2, count)
  }
}

def hits[Elem](list1: List[Elem], list2: List[Elem]): Int = {
  hits_helper(list1, list2, 0)
}


val list1 = List(1, 0, 1, 1, 1)
val list2 = List(1, 0, 0, 1, 1)
val test1 = hits(list1, list2)
println(test1)

val list3 = List('a', 'b', 'c', 'd')
val list4 = List('a', 'b', 'c', 'd')
val test2 = hits(list3, list4)
println(test2)

val listEmpty = List()
val test3 = hits(list3, listEmpty)



