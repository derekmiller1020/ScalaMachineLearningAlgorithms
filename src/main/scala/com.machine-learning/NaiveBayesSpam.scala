import scala.collection.mutable

object NaiveBayesSpam {

    //lower case and find words
    def tokenize(message: String) : Set[String] = "[a-z0-9']+".r.findAllIn(message.toLowerCase).toSet

    //Count words that are in spam messages and not spam messages
    def countWords(trainingSet: List[(String, Boolean)]) : Map[String, (Int, Int)] = {
        val counts = mutable.Map[String, (Int, Int)]().withDefaultValue((0,0)) //for each key, set a default value of (0,0)
        for ((message, isSpam) <- trainingSet){
            for (word <- tokenize(message)){ //get lower cased word
                val (spamVal, nonSpam) = counts(word)
                if (isSpam){
                    counts(word) = (spamVal + 1, nonSpam) //add the spam value
                } else{
                    counts(word) = (spamVal, nonSpam + 1) //add the non spam value
                }
            }
        }
        counts.toMap
    }

    //Probability that each word is spam and not spam, returns the word with (Double (prob of spam), Double(prob not spam))
    def wordProbabilities(counts: Map[String, (Int, Int)], totalSpam: Int, totalNonSpam: Int, k: Double) : Map[String, (Double, Double)] = {
        counts.map((value) => {
            val (word, (spamCount, nonSpamCount)) = value
            (word, (
                (spamCount + k) / (totalSpam + 2 * k ), //i.e (24 + .5) / (125 + 2 * .5) probability is spam
                (nonSpamCount + k) / (totalNonSpam + 2 * k) //i.e (10 + .5) / (139 + 2 * .5) probability isn't spam
                )
            )
        })
    }

    //determines if in upcoming message is spam based on training set
    def spamProbability(wordProbs: Map[String, (Double, Double)], message: String) : Double = {
        val messageWords = tokenize(message) //tokenize incoming message
        var logProbabilityIfSpam, logProbabilityIfNotSpam = 0.0
        for ((word, (probIfSpam, probIfNotSpam)) <- wordProbs){ //iterate through training set
            if (messageWords.contains(word)){ //don't need to worry about casing, since it is tokenized
                logProbabilityIfSpam += math.log(probIfSpam)
                logProbabilityIfNotSpam += math.log(probIfNotSpam)
            } else {
                logProbabilityIfSpam += math.log(1.0 - probIfSpam)
                logProbabilityIfNotSpam += math.log(1.0 - probIfNotSpam)
            }
        }
        val probSpam = math.exp(logProbabilityIfSpam)
        val probNotSpam = math.exp(logProbabilityIfNotSpam)
        probSpam / (probSpam + probNotSpam)
    }
}

class NaiveBayesClassifier(val k: Double) {

    //train the data
    def train(trainingSet: List[(String, Boolean)]) : Unit = {
        val numberOfSpams = trainingSet.count((value) => value._2)
        val numberOfNotSpams = trainingSet.count((value) => !value._2)
        val wordCounts = NaiveBayesSpam.countWords(trainingSet)
        val wordProbs = NaiveBayesSpam.wordProbabilities(wordCounts, numberOfSpams, numberOfNotSpams, k)
    }

    //find probability that object is spam
    def classify(message: String, wordProbs: Map[String, (Double, Double)]) = NaiveBayesSpam.spamProbability(wordProbs, message)

}
