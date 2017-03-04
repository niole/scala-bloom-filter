/**
 * n elements
 * k hashing functions
 * m bits in filter (size m)
 *
 * how to size a bloom filter: minimize error rate given n, m, k values by adjusting them and then checking the error rate
 **/

class BloomFilter(maxData: Int, errorRate: Double, totalHashFunctions: Int = 2) {

  private[this] var set = init

  private[this] def getErrorRate(k: Double, n: Double, m: Double): Double = Math.pow((1.0-Math.exp(-k*n/m)), k)

  private[this] def init: Seq[Int] = {
    var eRate = errorRate + 1
    var size = maxData - 10

    while (eRate > errorRate) {
     size += 10
     eRate = getErrorRate(totalHashFunctions, maxData, size)
    }

    Seq.fill(size){ 0 }

  }

  def get: Seq[Int] = set

}


object Main extends App {
  val bf = new BloomFilter(100, .05)
  println(bf.get)
}
