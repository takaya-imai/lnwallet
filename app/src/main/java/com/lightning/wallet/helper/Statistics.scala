package com.lightning.wallet.helper


abstract class Statistics[O] {
  type Collection = Traversable[O]
  def extract(item: O): Double

  def mean(items: Collection): Double =
    items.map(extract).sum / items.size

  def variance(items: Collection, mean: Double): Double = (0D /: items) {
    case (total: Double, item) => math.pow(extract(item) - mean, 2) + total
  } / items.size

  def filterWithin(items: Collection, stdDevs: Double) = {
    // Filter out items whose value of interest is inside
    // of stdDevs standard deviations

    val mean1 = mean(items)
    val sd = math sqrt variance(items, mean1)
    val (lowerBound, upperBound) = (mean1 - sd * stdDevs, mean1 + sd * stdDevs)
    items.filter(item => extract(item) >= lowerBound && extract(item) <= upperBound)
  }
}