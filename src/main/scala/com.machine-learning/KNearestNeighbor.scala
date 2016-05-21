

object KNearestNeighbor {

    def majorityVote(labels: List[String]) : String = {
        val countMap = labels.groupBy(l => l).map(t => (t._1, t._2.length))
        val highestCount = countMap.values.max
        val winners = countMap.toStream.filter((x) => x._2 == highestCount).toList
        if (winners.length == 1) {
            winners.head._1
        } else {
            majorityVote(labels.dropRight(1))
        }
    }

    def knnClassify(k: Int, labeledPoints : List[(List[Double], String)], newPoint: List[Double]) : String = {
        val byDistance = labeledPoints.sortWith((points, points2) => LinearAlgebra.distance(points._1, newPoint) < LinearAlgebra.distance(points2._1, newPoint))
        val kNearestLabels = byDistance.toStream.map((item) => item._2).take(k).toList
        majorityVote(kNearestLabels)
    }

    def example() : Unit = {
        List(1,3,5,7).foreach((k) => {
            var numberCorrect = 0
            SampleData.cities.foreach((item) => {
                val location = item._1
                val language = item._2
                val otherCities = SampleData.cities.filter((otherCity) => !otherCity.equals(location, language))
                val predictedLanguage = knnClassify(k, otherCities, location)
                if (predictedLanguage.equalsIgnoreCase(language)){
                    numberCorrect += 1
                }
            })
            println(k + " neighbors " + numberCorrect + " correct out of " + SampleData.cities.length)
        })
    }

    def main(args: Array[String]) : Unit = {
        example()
    }

}
