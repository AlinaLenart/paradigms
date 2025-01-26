package trafficlights

class YellowState extends State {
  def next: State = new RedState 

  override def toString: String =
    """---------
      ||   ◌   |
      ||   ●   |
      ||   ◌   |
      |---------
          """.stripMargin  
}
