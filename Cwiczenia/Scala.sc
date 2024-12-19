//zad 6.1
def whileLoop(condition: => Boolean)(body: => Unit): Unit = {
  if (condition) {
    body
    whileLoop(condition)(body)
  }
}// condition: => Boolean oraz body: => Unit to parametry leniwe 
//typ wyniku funkcji to Unit, ponieważ petla while nie zwraca wartosci
var count = 0
whileLoop(count < 3) {
  println(count)
  count += 1
}






//zad 6.2
//zamienia dwa elementy w tablicy
def swap(arr: Array[Int], i: Int, j: Int): Unit = {
  val temp = arr(i)
  arr(i) = arr(j)
  arr(j) = temp
}
//dzieli tablice na elementy mniejsze i wieksze od elementu obrotowego
def partition(arr: Array[Int], left: Int, right: Int, pivot: Int): Int = {
  var i = left
  var j = right
  while (i <= j) {
    while (arr(i) < pivot) i += 1
    while (arr(j) > pivot) j -= 1
    if (i <= j) {
      swap(arr, i, j)
      i += 1
      j -= 1
    }
  }
  i
}
//sortuje fragment tablicy
def quick(arr: Array[Int], left: Int, right: Int): Unit = {
  if (left < right) {
    val pivot = arr((left + right) / 2)
    val index = partition(arr, left, right, pivot)
    quick(arr, left, index - 1)
    quick(arr, index, right)
  }
}
def quicksort(arr: Array[Int]): Unit = {
  quick(arr, 0, arr.length - 1)
}






















//zad 5.1
def lrepeat[A](k:Int)(stream:Stream[A]):Stream[A] = {
    def repeatElement(element: A, reps:Int, rest:Stream[A]):Stream[A] = 
        if (reps == 0) rest
        else repeatElement(element, reps - 1, element #:: rest)
    stream match {
          case Stream.Empty => Stream.Empty
          case head #:: tail => repeatElement(head, k, lrepeat(k)(tail))
    }
}

//zad 5.2
val lfib = {
    def fibgen(a: Int, b: Int): Stream[Int] =
        a #:: fibgen(b, a + b)
    fibgen(0, 1)
}

//zad 5.3a
def lTree(n: Int): lBT[Int] =
LNode(n, () => lTree(2 * n), () => lTree(2 * n + 1))

//zad 5.3b
def toStream[A](tree: lBT[A]) = {
	def helper (queue: List[lBT[A]]): Stream[A] = queue match {
		case Nil => Stream.Empty
		case LEmpty :: tail => helper(tail)
		case LNode(value, leftSubtree, rightSubtree):: tail => value #:: helper(tail ++ List(leftSubtree(), rightSubtree()))
	}
	helper(List(tree))
}



























//zad 4.3
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

def breadthBT[A](tree: BT[A]) = {
	def helper[A](nodeQueue: List[BT[A]]): List[A] = nodeQueue match {
		case Nil => Nil
		case Empty :: tail => helper(tail)
		case Node(value, leftSubtree, rightSubtree) :: tail => value :: helper(tail ++ List(leftSubtree, rightSubtree))
	}
	helper (List(tree))
}

//zad 4.4a
def internalPath[A](tree: BT[A]) = {
	def helper (node: BT[A], depth: Int): Int = node match {
		case Empty => 0
		case Node(_, leftSubtree, rightSubtree) => depth + helper(leftSubtree, depth + 1) + helper(rightSubtree, depth + 1)
	}
	helper(tree, 0)
}

//zad 4.4b
def externalPath[A] (tree: BT[A]) = {
	def helper (node: BT[A], depth: Int): Int = node match {
		case Empty => depth
		case Node(_, leftSubtree, rightSubtree) => helper(leftSubtree, depth + 1) + helper(rightSubtree, depth + 1)
	}
	helper(tree, 0)

//zad 4.5
def depthSearch[A](g: Graph[A])(startNode: A) = {
  def search(visited: List[A])(toVisit: List[A]): List[A] = toVisit match {
	  case Nil => Nil
	  case h :: t =>
	    if (visited contains h) search(visited)(t) 
	    else h :: search(h :: visited)((g succ h) ++ t)
  }
  search(Nil)(List(startNode))
}
