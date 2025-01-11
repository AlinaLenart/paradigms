def pascalTriangle(n: Int): Array[Array[Int]] = {
  val triangle = Array.ofDim[Int](n, n)

  for (i <- 0 until n)
    for (j <- 0 to i)
      if (j == 0 || j == i) then
        triangle(i)(j) = 1
      else
        triangle(i)(j) = triangle(i - 1)(j - 1) + triangle(i - 1)(j)


  triangle
}

def pascalGift(n: Int): Array[Array[Int]] = {
  val size = 2 * n - 1
  val gift = Array.ofDim[Int](size, n)
  val triangle = pascalTriangle(n)

  // Górna część pudełka
  for (i <- n - 1 to 0 by -1)
    val side = Array.fill(n)(0)
    var x = i + 2
    var y = i + 1
    var idx = 0

    // Budowanie lewej strony
    while (x < n && y < n)
      side(idx) = triangle(x)(y)
      idx += 1
      x += 2
      y += 1

    // Lewa strona wiersza
    for (j <- 0 until idx)
      gift(n - 1 - i)(j) = side(j)

    // Środek wiersza
    for (j <- 0 to i)
      gift(n - 1 - i)(idx + j) = triangle(i)(j)

    // Prawa strona wiersza
    for (j <- 0 until idx)
      gift(n - 1 - i)(idx + i + 1 + j) = side(j)

  // Dolna część pudełka
  for (i <- 1 until n)
    val side = Array.fill(n)(0)
    var x = i + 2
    var y = i + 1
    var idx = 0

    // Budowanie lewej strony
    while (x < n && y < n)
      side(idx) = triangle(x)(y)
      idx += 1
      x += 2
      y += 1

    // Lewa strona wiersza
    for (j <- 0 until idx)
      gift(n - 1 + i)(j) = side(j)

    // Środek wiersza
    for (j <- 0 to i)
      gift(n - 1 + i)(idx + j) = triangle(i)(j)

    // Prawa strona wiersza
    for (j <- 0 until idx)
      gift(n - 1 + i)(idx + i + 1 + j) = side(j)

  gift
}

val gift5 = pascalGift(5)
val gift3 = pascalGift(3)
val gift1 = pascalGift(1)
val gift0 = pascalGift(0)
