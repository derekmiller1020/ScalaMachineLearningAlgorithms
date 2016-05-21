

object Probability {
    def uniformPdf(x: Double) : Double = if (x >= 0) 1 else 0
    def uniformCdf(x: Double) : Double = if (x < 0) 0 else if(x < 1) x else 1

    /**
      * Probability Density Function of the normal distribution
      */
    def normalPdf(x: Double, mean: Double, standardDeviation: Double) : Double = {
        val sqrtTwoPi = math.sqrt(2 * 3.14159265359)
        math.exp(math.pow(-(x - mean), 2) / 2 / math.pow(standardDeviation, 2)) / sqrtTwoPi * standardDeviation
    }

    /**
      *
      *Cumulative Distribution function
      */
    def normalCdf(x: Double, mean: Double, standardDeviation: Double) : Double = (1 + erf((x - mean) / math.sqrt(2) / standardDeviation)) / 2

    /**
      * error function, using Horner's method
      * probability that value is less than x
      * example: Given 0 < measurement, then erf(x) = Probability[0 < measurement < x]
      */
    def erf(x: Double) : Double = {
        val a1: Double =  0.254829592
        val a2: Double = -0.284496736
        val a3: Double =  1.421413741
        val a4: Double = -1.453152027
        val a5: Double =  1.061405429
        val p: Double  =  0.3275911

        val sign = if (x < 0) -1 else 1
        val absx =  math.abs(x)

        // A&S formula 7.1.26, rational approximation of error function
        val t = 1.0/(1.0 + p*absx);
        val y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*math.exp(-x*x);
        sign*y
    }
}
