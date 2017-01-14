import estimation.RobustEstimator
import geometry.{Line, Point}

import scala.util.Random
import scala.math.abs


object TestRANSAC extends App {

  val n_outliers = 100
  val n_inliers = 100
  val original_model = Line(Point.nextGaussian * 2.0)
  val sigma = 0.1
  val data = generate_data(n_outliers, n_inliers, original_model, sigma)

  val ransac = new RobustEstimator(pick_point_pair, get_line_from_point_pair, test_point_closer_than(3 * sigma))

  val iterations = 100
  val estimated_model = ransac.estimate(data, iterations)

  println(s"Original model $original_model")
  println(s"Estimated model: $estimated_model")

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

  def test_point_closer_than(threshold: Double)(l: Line, p: Point): Boolean = {
    abs(l distance p) < threshold
  }

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
      .map(_ => (Random.nextDouble - 0.5) * r)
      .map(x => Point(x, line(x)) + Point.nextGaussian * noise)

    outliers ++ inliers
  }
}
