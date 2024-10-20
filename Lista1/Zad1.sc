val fiddle4 = (tuple: (Double, Double, Double, Double)) => {
  (tuple._4, tuple._1, tuple._3, tuple._2)
} //nie wyciagam z krotki tylko zamieniam kolejnosc wnetrza

val result = fiddle4((1.3, 2.0, 3.1, 4.2))
println(result)  //(4.2, 1.3, 3.1, 2.0)
