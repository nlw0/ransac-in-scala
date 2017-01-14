package geometry

import scala.math.sqrt

trait Vec2d {
  def x: Double

  def y: Double

  def -(that: Point): Point = Point(this.x - that.x, this.y - that.y)

  def +(that: Point): Point = Point(this.x + that.x, this.y + that.y)

  def *(that: Point): Double = this.x * that.x + this.y * that.y

  def cross(that: Point): Double = this.x * that.y - this.y * that.x

  def *(that: Double): Point = Point(this.x * that, this.y * that)

  def /(that: Double): Point = Point(this.x / that, this.y / that)

  def sqnorm = x * x + y * y

  def norm = sqrt(sqnorm)
}
