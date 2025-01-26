package trafficlights

class GreenState extends State {
  def next: State = new YellowState

  override def toString: String =
    """---------
      ||   ◌   |
      ||   ◌   |
      ||   ●   |
      |---------
      """.stripMargin  
}

