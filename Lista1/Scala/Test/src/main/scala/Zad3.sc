def insert_helper[Elem](list: List[Elem], element: Elem, index: Int, count: Int): List[Elem] = {
  if (count == index) {
    element :: list
  }
  else if (list == Nil) {
    List(element)
  }
  else {
    list.head :: insert_helper(list.tail, element, index, count + 1)
  }
}

def insert[Elem](list: List[Elem], element: Elem, index: Int): List[Elem] = {
  if (index < 0) {
    throw new IllegalArgumentException("BLAD: indeks tablicy < 0")
  }
  else {
    insert_helper(list, element, index, 0)
  }
}


val list1 = List(1, 2, 3, 4, 5)
val elem1 = 0
val index1 = 0
val test1 = insert(list1, elem1, index1)
println(test1)  

val list2 = List(2, 3, 4)
val elem2 = 10
val index2 = 6
val test2 = insert(list2, elem2, index2)
println(test2)  

try {
  val test3 = insert(list2, 10, -3)  //10 na indeks -3 (wyjatek)
} catch {
  case e: IllegalArgumentException => println(e.getMessage)
}
