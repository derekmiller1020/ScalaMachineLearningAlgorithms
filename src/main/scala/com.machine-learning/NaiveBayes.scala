import com.github.tototoshi.csv.CSVReader
import scala.math._


object NaiveBayes {

    def main(args: Array[String]) : Unit = {
        println(
            predict(
                summarizeByClass(
                    separateByClass(
                        splitData(loadCSV("sample_data.csv"), 0.8)._1
                    )
                ), List(6,148,72,35,0,33.6,0.627,50,1)
            )
        )
    }

    //load csv to Collection of lists of doubles
    def loadCSV(resourceName: String): List[List[Double]] = CSVReader.open(resourceName).all().toStream
        .map((x) => x.toStream.map(y => y.toDouble).toList)
        .toList

    //Split data for testing and training
    def splitData(data: List[List[Double]], amountSplit: Double) : (List[List[Double]], List[List[Double]]) = {
        val nums = data.grouped((data.length * amountSplit).toInt).toList
        (nums.head, nums.last)
    }

    //separate by each classifier. In this case, the last double in the data set is the classifier (1, 0)
    def separateByClass(dataSet: List[List[Double]]) : Map[Double, List[List[Double]]] = {
        val itemMap: scala.collection.mutable.Map[Double, List[List[Double]]] = scala.collection.mutable.Map()
        dataSet.toStream.foreach(items => {
            itemMap(items.last) = if (itemMap.get(items.last).isEmpty) List(items) else itemMap.get(items.last).get :+ items
        })
        itemMap.toMap
    }

    //Total amount divided by size
    def mean(xs: List[Double]): Double = xs match {
        case Nil => 0.0
        case ys => ys.reduceLeft(_ + _) / ys.size.toDouble
    }

    //Square root summation of mean minus variance squared
    def standardDeviation(xs: List[Double], avg: Double): Double = xs match {
        case Nil => 0.0
        case ys => math.sqrt((0.0 /: ys) {
            (a,e) => a + math.pow(e - avg, 2.0)
        } / xs.size)
    }

    //summarize total data
    def summarize(nums: List[List[Double]]) : List[(Double, Double)] =
        nums.transpose.toStream
            .map(item => (mean(item), standardDeviation(item, mean(item))))
            .toList.dropRight(1)

    //summarize each class
    def summarizeByClass(items : Map[Double, List[List[Double]]]) : Map[Double, List[(Double, Double)]] =
        Map(0D -> summarize(items.get(0D).get), 1D -> summarize(items.get(1D).get))

    //calculates probability of single class
    def calculateProbability(guessValue: Double, mean: Double, stdev: Double) : Double =
        (1 / (sqrt(2 * 3.141592653589793) * stdev)) * exp(-(pow(guessValue - mean, 2)/(2 * pow(stdev,2))))

    //calculates probility of both classes
    def calculateClassProbabilities(summaries: Map[Double, List[(Double, Double)]], inputData: List[Double]) : Map[Double, Double] = {
        val probabilities : scala.collection.mutable.Map[Double, Double] = scala.collection.mutable.Map()
        summaries.foreach{case (key, classSummaries) => {
            probabilities(key) = 1
            for (i <- 0 until classSummaries.length){
                val (meanVal, standardDeviationVal) = classSummaries(i)
                val x = inputData(i)
                probabilities(key) = calculateProbability(x, meanVal, standardDeviationVal)
            }
        }}
        probabilities.toMap
    }

    //makes prediction
    def predict(summaries: Map[Double, List[(Double, Double)]], inputData: List[Double]) : (Double, Double) = {
        val probabilites = calculateClassProbabilities(summaries, inputData)
        probabilites.reduceLeft((x,y) => if (x._2 > y._2) x else y)
    }

}