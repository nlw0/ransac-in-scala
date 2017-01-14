package geometry

case class Line(override val x: Double, override val y: Double) extends Vec2d {
  def distance(p: Point) = {
    val lp = Point(x, y)
    val proj = lp * (lp * p) / lp.sqnorm
    (proj - lp) * lp / lp.norm
  }

  def apply(ix: Double) = {
    y + x * (x - ix) / y
  }
}

object Line {
  def apply(p: Point): Line = Line(p.x, p.y)
}