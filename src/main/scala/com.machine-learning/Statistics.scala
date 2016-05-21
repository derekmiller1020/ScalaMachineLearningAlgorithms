

object Statistics {
    def mean(items : List[Double]) : Double = items.sum / items.length
    def deviationMean(items : List[Double]) : List[Double] = {
        val xMean = mean(items)
        items.map((item) => item - xMean)
    }
    //sum of squared deviations divided by length of items - 1
    def variance(items: List[Double]) : Double = LinearAlgebra.sumOfSquares(deviationMean(items)) / (items.length - 1)
    def standardDeviation(items: List[Double]) : Double = math.sqrt(variance(items))
    //variance in relation to two vectors
    def covariance(first: List[Double], second: List[Double]) : Double = LinearAlgebra.dot(deviationMean(first), deviationMean(second)) / (first.length -1)
    def correlation(first: List[Double], second: List[Double]) : Double = {
        val standardDeviationFirst = standardDeviation(first)
        val standardDeviationSecond = standardDeviation(second)
        if (standardDeviationFirst > 0 && standardDeviationSecond > 0) covariance(first, second) / standardDeviationFirst / standardDeviationSecond  else 0
    }
}
