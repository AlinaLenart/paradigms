def factors(n: Int): List[Int] =
  for (
    k <- List.range(1, n + 1)
    if n % k == 0
  ) yield k

def composed(n: Double): List[Int] =
  val upperLimit = n.toInt // ucinam czesc ulamkowa
  for (
    x <- List.range(2, upperLimit + 1) // od 1 nie ma co
    if factors(x).length > 2
  ) yield x


composed(10)
composed(7.9)
composed(0)
composed(-2.1)






















//def composed(n: Int): List[Int] = {
//  for (
//    x <- List.range(2, n + 1);
//    if (
//      for (k <- List.range(2, x); if x % k == 0)
//      yield k).nonEmpty)
//  yield x
//}
