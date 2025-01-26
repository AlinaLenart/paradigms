package trafficlights

class RedYellowState extends State {
  def next: State = new GreenState

  override def toString: String =
    """---------
      ||   ●   |
      ||   ●   |
      ||   ◌   |
      |---------
        """.stripMargin
}
