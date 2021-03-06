import java.awt.Color

import breeze.linalg.DenseVector
import breeze.plot._
import estimation.RobustEstimator
import geometry.{Line, Point}

import scala.math.abs
import scala.util.Random

object PlotRANSAC extends App {

  val n_outliers = 100
  val n_inliers = 100
  val aux = Line(Point.nextGaussian)
  val original_model = if (abs(aux.x) < abs(aux.y)) aux else Line(aux.y, aux.x)

  val sigma = 0.2
  val data = generate_data(n_outliers, n_inliers, original_model, sigma)

  val ransac = new RobustEstimator(pick_point_pair, get_line_from_point_pair, test_point_closer_than(3 * sigma))

  val iterations = 10
  val estimated_model = ransac.estimate(data, iterations)

  println(s"Original model $original_model")
  println(s"Estimated model: $estimated_model")

  val xx = breeze.linalg.DenseVector[Double]((data map { p => p.x }).toArray)
  val yy = breeze.linalg.DenseVector[Double]((data map { p => p.y }).toArray)

  val fig = Figure()
  val plt = fig.subplot(0)
  plt += scatter(xx, yy, { _ => 0.1 }, colors = { _ => Color.BLACK })
  val aa = original_model(-4)
  val bb = original_model(4)
  val cc = estimated_model(-4)
  val dd = estimated_model(4)
  plt += plot(DenseVector[Double](-4, 4), DenseVector[Double](aa, bb), style = '-', colorcode = "b")
  plt += plot(DenseVector[Double](-4, 4), DenseVector[Double](cc, dd), style = '-', colorcode = "r")


  def pick_point_pair(data: Seq[Point]): (Point, Point) = {
    val List(p1, p2) = sample_without_replacement(2, data)
    (p1, p2)
  }

  def get_line_from_point_pair(point_pair: (Point, Point)): Line = {
    val (p1, p2) = point_pair
    val delta = p2 - p1
    val x = delta.y * (p1 cross p2) / delta.sqnorm
    val y = -delta.x * (p1 cross p2) / delta.sqnorm
    Line(x, y)
  }

  def test_point_closer_than(threshold: Double)(l: Line)(p: Point): Boolean =
    abs(l distance p) < threshold

  def sample_without_replacement[D](N: Int, data: Seq[D], sample: List[D] = List.empty): List[D] = {
    if (N <= 0) sample else {
      val el = Random.nextInt(data.size)
      val remaining_data = data.take(el) ++ data.drop(el + 1)
      sample_without_replacement(N - 1, remaining_data, sample :+ data(el))
    }
  }

  def generate_data(n_outliers: Int, n_inliers: Int, line: Line, noise: Double) = {
    val r = 10.0 // Size of test space

    val outliers = (1 to n_outliers) map { _ => (Point.nextUniform - Point(0.5, 0.5)) * r }
    val inliers = (1 to n_inliers)
      .map(_ => (Random.nextDouble - 0.5) * r / 2)
      .map(x => Point(x, line(x)) + Point.nextGaussian * noise)

    outliers ++ inliers
  }
}
