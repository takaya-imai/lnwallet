package com.lightning.wallet.helper


abstract class Statistics[O] {
  type Collection = Traversable[O]
  def extract(item: O): Double

  def μ(items: Collection): Double =
    items.map(extract).sum / items.size

  def σ(items: Collection, mean: Double): Double = (0D /: items) {
    case (total: Double, item) => math.pow(extract(item) - mean, 2) + total
  } / items.size

  def filterWithin(items: Collection, stdDevs: Double) = {
    // Filter out items whose value of interest is outside
    // of stdDevs standard deviations

    val mean = μ(items)
    val sd = math sqrt σ(items, mean)
    val (lowerBound, upperBound) = (mean - sd * stdDevs, mean + sd * stdDevs)
    items.filter(item => extract(item) > lowerBound && extract(item) < upperBound)
  }
}
