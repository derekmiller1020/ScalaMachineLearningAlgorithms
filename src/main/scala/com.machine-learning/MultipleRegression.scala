import scala.util.Random


/**
  * multiple regression model: y = beta0 + beta1 * x1 + beta2 * x2 ... + error
  * multiple regression estimate: y = beta0 + beta1 * x1 + beta2 * x2 ...
  */
object MultipleRegression {
    //assumed xI is constant of 1 and slope first contains alpha or beta0 (y intercept), function multiplies each value (ex: x1 * beta1 + ...)
    def predict(xI : List[Double], slope: List[Double]) : Double = LinearAlgebra.dot(xI, slope)
    //when building the model, the error is made from subtracting y (the actual dependent variable) with its prediction
    def error(xI: List[Double], yI: Double, slope: List[Double]) : Double = yI - predict(xI, slope)
    //square error, which ensures positive number and can have a derivative
    def squaredError(xI: List[Double], yI: Double, slope: List[Double]) : Double = error(xI, yI, slope)
    //find the squared error gradient
    def squaredErrorGradient(xI: List[Double], yI: Double, slope: List[Double]) : List[Double] = xI.map((xIJ) => -2 * xIJ * error(xI, yI, slope))
    //estimate beta given a list of lists of x and a list of y (ex pair would be like (y1, [xList contents])
    def estimateBeta(x: List[List[Double]], y: List[Double]) : List[Double] = {
        val betaInitial = x.head.map((xI) => Random.nextDouble())
        GradientDescent.minimizeStochastic(
            squaredError, squaredErrorGradient, x, y, betaInitial
        )
    }

    //determines how closely the points are to the regression line
    def multipleRSquared(x: List[List[Double]], y: List[Double], beta: List[Double]) : Double = {
        val sumOfSquaredErrors = x.zip(y).map((combined) => Math.pow(error(combined._1, combined._2, beta), 2)).sum
        1.0 - sumOfSquaredErrors / SimpleLinearRegression.totalSumOfSquares(y)
    }


    def main(args: Array[String]) : Unit = {
        val x = SampleData.x.take(35)
        val dailyMinutesGood = SampleData.daily_minutes_good.take(35)

        val beta = estimateBeta(x, dailyMinutesGood)
        println(" beta " + beta)
        println(" r squared " + multipleRSquared(x, dailyMinutesGood, beta))

        val testData2 : List[List[Double]] = List(List(1, 50, 80), List(1, 80,65), List(1, 60,60), List(1, 95,80), List(1, 95,50), List(1, 40,90))
        val classifiers : List[Double] = List(65, 83, 69, 92, 84, 55)
        val beta2 = estimateBeta(testData2, classifiers)
        println("school beta " + beta2)
        println( " r squared school" + multipleRSquared(testData2, classifiers, beta))
    }


}
