package trafficlights

class TrafficLights {
  private var currentState: State = new RedState //zaczynam od czerwonego

  def changeState(): Unit = {
    currentState = currentState.next 
  }

  override def toString: String = currentState.toString 
}
