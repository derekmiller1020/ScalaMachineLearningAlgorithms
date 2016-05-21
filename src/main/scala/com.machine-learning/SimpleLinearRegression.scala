
/**
  * Simple linear regression
  * ex model: y = slope * x + y-intercept + error
  */
object SimpleLinearRegression {
    def predict(yIntercept: Double, slope: Double, xI: Double) : Double = (slope * xI) + yIntercept
    //error rate of line with alpha (yintercept) and beta(slope)
    def error(yIntercept: Double, slope: Double, xI: Double, yI: Double) : Double = yI - predict(yIntercept, slope, xI)
    def sumOfSquaredErrors(yIntercept: Double, slope: Double, x: List[Double], y: List[Double]) :  Double = x.zip(y).map((value) => math.pow(error(yIntercept, slope, value._1, value._2), 2)).sum

    //find y-intercept and slope, given x and y
    def leastSquaresFit(first: List[Double], second: List[Double]) : (Double, Double) = {
        val slope = Statistics.correlation(first, second) * Statistics.standardDeviation(second) / Statistics.standardDeviation(first)
        val yIntercept = Statistics.mean(second) - (slope * Statistics.mean(first))
        (slope, yIntercept)
    }

    // the total squared variation of seconds's from their mean
    def totalSumOfSquares(second : List[Double]) : Double = Statistics.deviationMean(second).map((item) => math.pow(item, 2)).sum

    //The fraction of variation in y captured by the model, which equals 1 - the fraction of variation in y not captured by the model
    //determines how closely the points are to the regression line.
    def rSquared(yIntercept: Double, slope: Double, x: List[Double], y: List[Double]) : Double = 1.0 - (sumOfSquaredErrors(yIntercept, slope, x, y) / totalSumOfSquares(y))

    def squaredError(xI: Double, yI: Double, theta: (Double, Double)) : Double = {
        val (yIntercept, slope) = theta
        Math.pow(error(yIntercept, slope, xI, yI), 2)
    }

    def squaredErrorGradient(xI: Double, yI: Double, theta: (Double, Double)) : (Double, Double) = {
        val (yIntercept, slope) = theta
        (-2 * error(yIntercept, slope, xI, yI), //yIntercept partial derivative
            -2 * error(yIntercept, slope, xI, yI) * xI) //slope partial derivative
    }

    def main(args: Array[String]) : Unit = {
        val (slope, yIntercept) = leastSquaresFit(SampleData.numFriendsGood, SampleData.dailyMinutesGood)
        println("Slope (beta): " + slope)
        println("yIntercept (alpha): " + yIntercept)
        println("r-squared: " + rSquared(yIntercept, slope, SampleData.numFriendsGood, SampleData.dailyMinutesGood))
    }
}
