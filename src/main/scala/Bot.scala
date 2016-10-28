import util.Random

class ControlFunctionFactory {
  def create = new ControlFunction().respond _
}

class ControlFunction {
  val rnd = new Random()

  def respond(input: String): Any = {
    val (opcode, paramMap) = CommandParser(input)

    if( opcode == "React" ) {
      val viewString = paramMap("view")
      val view = View(viewString)
      view.offsetToNearest('B') match {
        case Some(offset) =>
          val unitOffset = offset.signum

          val evil = Array('b', 'p', 'W')
          var move = unitOffset
          while( evil.contains(view.cellAtRelPos(move)) ) {
            move = XY.Directions(Random.nextInt(XY.Directions.length))
          }
          "Move(direction=" + move + ")|Status(text="+move+")"

        case None =>
          view.offsetToNearest('P') match {
            case Some(offset) =>
              val unitOffset = offset.signum

              val evil = Array('b', 'p', 'W')
              var move = unitOffset
              while( evil.contains(view.cellAtRelPos(move)) ) {
                move = XY.Directions(Random.nextInt(XY.Directions.length))
              }
              "Move(direction=" + move + ")|Status(text="+move+")"

            case None =>
              var move = XY.Directions(Random.nextInt(XY.Directions.length))
              while ( view.cellAtRelPos(move) == 'W') {
                move = move.rotateClockwise45
              }
              "Move(direction=" + move + ")|Status(text="+move+")"
          }
      }
    } else ""
  }
}
