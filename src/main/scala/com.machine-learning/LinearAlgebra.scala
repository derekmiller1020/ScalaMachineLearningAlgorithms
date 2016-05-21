import scala.math._


object LinearAlgebra {
    //subtract two vectors
    def vectorSubtract(v: List[Double], w: List[Double]) : List[Double] = v.zip(w).map((item) => item._1 - item._2)
    //add two vectors
    def vectorAdd(v: List[Double], w: List[Double]) : List[Double] = v.zip(w).map((item) => item._1 + item._2)
    //zip vectors then multiply ith items. ex [3,4,5], [4,2,5] = 12 + 8 + 25
    def dot(v: List[Double], w: List[Double]) : Double = v.zip(w).map((item) => item._1 * item._2).sum
    //add all squared data
    def sumOfSquares(v: List[Double]) : Double = dot(v, v)
    def squaredDistance(v: List[Double], w: List[Double]) : Double = sumOfSquares(vectorSubtract(v, w))
    def distance(v: List[Double], w: List[Double]) : Double = math.sqrt(squaredDistance(v,w))
    def scalarMultiply(c: Double, v: List[Double]) : List[Double] = v.map((vI) => vI * c)
}
