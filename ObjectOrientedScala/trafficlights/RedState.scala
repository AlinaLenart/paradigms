package trafficlights

class RedState extends State {
  def next: State = new RedYellowState

  override def toString: String =
    """---------
      ||   ●   |
      ||   ◌   |
      ||   ◌   |
      |---------
    """.stripMargin

}
