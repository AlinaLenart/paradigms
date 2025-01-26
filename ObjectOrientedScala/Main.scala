package trafficlights

object Main {
  def main(args: Array[String]): Unit = {

    val trafficLights = new TrafficLights

    println("Initial State:")
    println(trafficLights)

    for (_ <- 1 to 4) {
      trafficLights.changeState()
      println("After Change:")
      println(trafficLights)
    }
  }
}
