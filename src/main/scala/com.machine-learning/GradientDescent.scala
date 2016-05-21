import scala.annotation.tailrec
import scala.util.Random

object GradientDescent {
    def sumOfSquares(v: List[Double]) : Double = v.map((value) => value * value).sum
    def differenceQuotient(f: (Double) => Double, x: Double, h: Double) : Double = (f(x+h) - f(x)) / h
    //find the sum of squares gradient
    def sumOfSquaresGradient(values: List[Double]) : List[Double] = values.map((value) => 2 * value)

    /**
      * Step towards minimum for lowest gradient
      */
    def step(values: List[Double], direction: List[Double], stepSize: Double) : List[Double] = values.zip(direction).map((combined) => combined._1 + stepSize * combined._2)

    def minimizeBatch(targetFunction: (List[Double]) => Double, gradientFunction: (List[Double]) => List[Double], theta_0: List[Double]) : List[Double] = {
        //constants
        val tolerance = 0.000000001
        val stepSizes : List[Double] = List(100, 10, 1, .1, .01, .001, .0001, .00001)

        //set theta and value
        val theta = theta_0
        //sum of squares
        val value = targetFunction(theta)

        @tailrec
        def innerFunc(thetaI: List[Double], valueI : Double) : List[Double] = {
            val gradient = gradientFunction(thetaI)
            //get a list of lists of potential step values
            val nextThetas = stepSizes.map((stepSize) => step(thetaI, gradient, -stepSize))
            //then sort the potential steps based on sum of squares and grab the smallest number (head)
            val nextTheta = nextThetas.sortWith((item1, item2) => targetFunction(item1) < targetFunction(item2)).head
            val nextValue = targetFunction(nextTheta)
            if (Math.abs(valueI - nextValue) < tolerance){
                thetaI
            } else {
                innerFunc(nextTheta, nextValue)
            }
        }
        innerFunc(theta, value)
    }

    @tailrec
    def basicGradientDescent(gradientFunction: (List[Double]) => List[Double], v: List[Double]) : List[Double] = {
        val tolerance = 0.000000001
        val gradient = gradientFunction(v) //compute gradient at v
        val nextV = step(v, gradient, -0.01) // take a negative gradient step
        if (LinearAlgebra.distance(nextV, v) < tolerance){
            v
        } else {
            //keep going until the distance is below tolerance
            basicGradientDescent(gradientFunction, nextV)
        }
    }

    def inRandomOrder(data: List[(List[Double], Double)]) : List[(List[Double], Double)] = new Random().shuffle(data)

    /**
      * stochastic gradient descent
      */
    def minimizeStochastic(
                              targetFunction: (List[Double], Double, List[Double]) => Double,
                              gradientFunction: (List[Double], Double, List[Double]) => List[Double],
                              x: List[List[Double]],
                              y: List[Double],
                              theta0: List[Double]) : List[Double] = {
        val data = x.zip(y)

        var theta = theta0 // initial guess
        var stepSize = 0.01 //initial stepSize
        var minTheta : List[Double] = Nil // no value
        var minValue = Double.MaxValue //set the max
        var iterationsWithNoImprovement = 0

        //if we ever go 100 iterations with no improvements then stop
        while (iterationsWithNoImprovement < 100){
            val value = data.map((combined) => targetFunction(combined._1, combined._2, theta)).sum
            if (value < minValue){
                //if we have found a new minimum, remember it
                //and go back to original step size
                minTheta = theta
                minValue = value
                iterationsWithNoImprovement = 0
                stepSize = 0.01
            } else {
                iterationsWithNoImprovement += 1
                stepSize *= 0.9
            }
        }

        inRandomOrder(data).foreach((combined) => {
            val gradientI = gradientFunction(combined._1, combined._2, theta)
            theta = LinearAlgebra.vectorSubtract(theta, LinearAlgebra.scalarMultiply(stepSize, gradientI))
        })

        minTheta
    }


    def main(args: Array[String]) : Unit = {
        val v: List[Double] = List(-1, 6, 2)
        println("== Using the gradient ==")
        val z = basicGradientDescent(sumOfSquaresGradient, v)
        println("minimum z " + z)
        println("minimum value " + sumOfSquares(z))
        println(" ")
        println("===using minimize batch ====")
        val x = minimizeBatch(sumOfSquares, sumOfSquaresGradient, v)
        println("min x " + x)
        println("minimum value " + sumOfSquares(x))
    }
}
