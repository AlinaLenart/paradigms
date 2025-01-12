def nextRow(row: List[Int]): List[Int] = {
  def sumPairs(list1: List[Int], list2: List[Int], acc: List[Int]): List[Int] = {
    (list1, list2) match {
      case (x :: xs, y :: ys) => sumPairs(xs, ys, acc :+ (x + y))
      case _ => acc
    }
  }
  val rowLeftZeros = 0 :: row
  val rowRightZeros = row :+ 0
  sumPairs(rowLeftZeros, rowRightZeros, List.empty)
}

def generateRows(n: Int): List[List[Int]] = {
  def build(idx: Int, triangle: List[List[Int]]): List[List[Int]] = {
    if idx == n then triangle.reverse
    else triangle match {
      case Nil => build(idx + 1, List(List(1)))
      case lastRow :: _ =>
        val newRow = nextRow(lastRow)
        build(idx + 1, newRow :: triangle)
    }
  }
  build(0, List.empty)
}

def pascalTriangle(n: Int): List[List[Int]] = generateRows(n)

def getElementAtIndex[A](list: List[A], index: Int): A = {
  def helper(idx: Int, lst: List[A]): A = {
    lst match {
      case Nil => throw new Exception("Index out of bound")
      case x :: xs => if idx == 0 then x
        else helper(idx - 1, xs)
    }
  }
  helper(index, list)
}

//potrzebne do edgeElements
def collectElements(rowIndex: Int, colIndex: Int, n: Int, triangle: List[List[Int]]): List[Int] = {
  def helper(acc: List[Int], x: Int, y: Int): List[Int] = {
    if x < n && y < triangle(x).length then
      helper(triangle(x)(y) :: acc, x + 2, y + 1)
    else
      acc.reverse
  }
  helper(List.empty, rowIndex, colIndex)
}

//gorna czesc prezentu
def buildTop(n: Int, triangle: List[List[Int]]): List[List[Int]] = {
  def helper(acc: List[List[Int]], i: Int): List[List[Int]] = {
    if i < 0 then acc.reverse
    else {
      val edgeElements = collectElements(i + 2, i + 1, n, triangle)
      val leftSide = edgeElements.reverse
      val middle = triangle(i)
      val rightSide = edgeElements
      val fullRow = leftSide ++ middle ++ rightSide
      helper(fullRow :: acc, i - 1)
    }
  }
  helper(List.empty, n - 1)
}

//dolna czesc prezentu
def buildBottom(n: Int, triangle: List[List[Int]]): List[List[Int]] = {
  def helper(acc: List[List[Int]], i: Int): List[List[Int]] = {
    if i >= n then acc.reverse
    else {
      val edgeElements = collectElements(i + 2, i + 1, n, triangle)
      val leftSide = edgeElements.reverse
      val middle = triangle(i)
      val rightSide = edgeElements
      val fullRow = leftSide ++ middle ++ rightSide
      helper(fullRow :: acc, i + 1)
    }
  }
  helper(List.empty, 1)
}

def pascalGift(n: Int): List[List[Int]] = {
  val triangle = pascalTriangle(n)
  val topPart = buildTop(n, triangle)
  val bottomPart = buildBottom(n, triangle)
  topPart ++ bottomPart
}

pascalGift(5);
pascalGift(4);
pascalGift(3);
pascalGift(2);
pascalGift(1);
pascalGift(0);