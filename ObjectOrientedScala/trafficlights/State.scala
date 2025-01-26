package trafficlights

trait State { 
  def next: State 
  override def toString: String 
}
