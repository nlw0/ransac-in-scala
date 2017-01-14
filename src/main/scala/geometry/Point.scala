package geometry

import scala.util.Random

case class Point(override val x: Double, override val y: Double) extends Vec2d

object Point {
  def nextGaussian = Point(Random.nextGaussian, Random.nextGaussian)

  def nextUniform = Point(Random.nextDouble, Random.nextDouble)
}