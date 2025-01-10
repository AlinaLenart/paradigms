sealed trait LazyTree[+A]
case object LEmpty extends LazyTree[Nothing]
case class LNode[A](value: A,
                      left: () => LazyTree[A],
                      middle: () => LazyTree[A],
                      right: () => LazyTree[A]) extends LazyTree[A]

//sealed trait LList[+A]
//case object LNil extends LList[Nothing]
//case class LCons[A](head: A, tail: () => LList[A]) extends LList[A]

def traverse[A](order: LazyTree[A] => List[() => LazyTree[A]], tree: LazyTree[A]): LazyList[A] = {
  def process_tree(t: LazyTree[A]): LazyList[A] = 
    t match {
      case LEmpty => LazyList()
      case LNode(value, left, middle, right) =>
        val children = order(t)
        value #:: process_children(children)
  }

  def process_children(children: List[() => LazyTree[A]]): LazyList[A] =
    children match { //lista leniwych funkcji wskazujacych na dzieci wezla
      case Nil => LazyList()
      case child :: rest =>
        child() match {
          case LEmpty => process_children(rest)
          case node => append_lists(process_tree(node), () => process_children(rest))
        }
  }

  def append_lists(l1: LazyList[A], l2: () => LazyList[A]): LazyList[A] =
    l1 match {
      case LazyList() => l2()
      case head #:: tail => head #:: append_lists(tail, l2)
  }

  process_tree(tree)
}


def to_list[A](llist: LazyList[A]): List[A] =
  llist match {
    case LazyList() => List()
    case head #:: tail => head :: to_list(tail)
}

val tree: LazyTree[Int] = LNode(1,
  () => LNode(2,
    () => LNode(5, () => LEmpty, () => LEmpty, () => LEmpty),
    () => LNode(6, () => LEmpty, () => LEmpty, () => LEmpty),
    () => LEmpty),
  () => LNode(3, () => LEmpty, () => LEmpty, () => LEmpty),
  () => LNode(4, () => LEmpty, () => LEmpty, () => LEmpty)
)

val tree_empty: LazyTree[Int] = LEmpty

def left_first[A](tree: LazyTree[A]): List[() => LazyTree[A]] =
  tree match {
    case LEmpty => Nil
    case LNode(_, left, middle, right) => List(left, middle, right)
}

def right_frist[A](tree: LazyTree[A]): List[() => LazyTree[A]] =
  tree match {
    case LEmpty => Nil
    case LNode(_, left, middle, right) => List(right, middle, left)
}


val resultLazyList1 = traverse(left_first, tree)
val resultList1 = to_list(resultLazyList1)

val resultLazyList2 = traverse(right_frist, tree)
val resultList2 = to_list(resultLazyList2)

val resultLazyListEmpty = traverse(left_first, tree_empty)
val resultListEmpty = to_list(resultLazyListEmpty)

def inifinityTree: LazyTree[Int] =
  LNode(1,
    () => inifinityTree,
    () => inifinityTree,
    () => inifinityTree)

val tree_infinity: LazyTree[Int] = inifinityTree

val resultLazyListInfinity = traverse(left_first, tree_infinity)
val resultListInfinity = to_list(resultLazyListInfinity)
