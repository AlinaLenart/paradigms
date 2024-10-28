val fiddle4 = (tuple: (Double, Double, Double, Double)) => {
  (tuple._4, tuple._1, tuple._3 - tuple._2) } 

val result = fiddle4((1.3, 2.0, 3.1, 4.2))
println(result)  //(4.2, 1.3, 1.1)
