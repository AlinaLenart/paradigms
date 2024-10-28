def militaryMinutes(hours: Int, minutes: Int, am_pm: String): String = {
  if (hours < 0 || hours > 12) {
    throw new IllegalArgumentException("BLAD: Godzina musi byc z zakresu 0-12")
  }
  else if (minutes < 0 || minutes >= 60) {
    throw new IllegalArgumentException("BLAD: Minuty musza byc z zakresu 0-59")
  }
  else {
    val militaryHours =
      if (am_pm == "AM") {
        if (hours == 12) 0 else hours
      } else if (am_pm == "PM") {
        if (hours == 12) 12 else hours + 12
      } else hours

    f"$militaryHours%2d : $minutes%02d"
  }
}


println(militaryMinutes(9, 5, "AM"))   //"9 : 05"
println(militaryMinutes(12, 12, "PM"))  //"12 : 21"
println(militaryMinutes(12, 0, "AM"))    //"0 : 00"
println(militaryMinutes(9, 5, "AM"))    //"21 : 05"

try {
  println(militaryMinutes(17, 1, "AM"))
} catch {
  case e: IllegalArgumentException => println(e.getMessage)
}