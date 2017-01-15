package estimation

class RobustEstimator[Data, Hypothesis, Model](sampler: Seq[Data] => Hypothesis,
                                               model_generator: Hypothesis => Model,
                                               inlier_detector: (Model, Data) => Boolean) {
  def estimate(data: Seq[Data], iterations: Int): Model = {
    val minimal_sets = Seq.fill(iterations) {sampler(data)}
    val hypothetical_models = minimal_sets map model_generator
    hypothetical_models.maxBy(m => data count (inlier_detector(m, _)))
  }
}
