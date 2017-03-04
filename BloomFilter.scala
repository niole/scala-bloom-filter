import scala.util.hashing.{ MurmurHash3, ByteswapHashing, Hashing }
import MurmurHash3.ArrayHashing

/**
 * n elements
 * k hashing functions
 * m bits in filter (size m)
 *
 * how to size a bloom filter: minimize error rate given n, m, k values by adjusting them and then checking the error rate
 **/

object BloomFilterSpec {

  class BloomFilter(maxData: Int, errorRate: Double, totalHashFunctions: Int = 2) {
    private val hashFunctions: Seq[(Int) => Int] = Seq((x: Int) => new ByteswapHashing[Int]().hash(x), (x: Int) => new ArrayHashing[Int]().hash(intToArray(x)))

    private[this] var set = init

    private[this] def getErrorRate(k: Double, n: Double, m: Double): Double = Math.pow((1.0-Math.exp(-k*n/m)), k)

    private def intToArray(n: Int): Array[Int] = n.toString.map(_.toInt).toArray

    private[this] def init: Array[Int] = {
      var eRate = errorRate + 1
      var size = maxData - 10

      while (eRate > errorRate) {
       size += 10
       eRate = getErrorRate(totalHashFunctions, maxData, size)
      }

      Array.fill(size){ 0 }

    }

    def getAll: Seq[Int] = set

    def add(number: Int): Unit = {

      hashFunctions.foreach(f => {
        set(Math.abs(f(number)) % set.length) = 1
      })

    }

    def contains(number: Int): Boolean = hashFunctions.forall(f => set(Math.abs(f(number)) % set.length) == 1)

  }

}


object Main extends App {
  import BloomFilterSpec._

  val bf = new BloomFilter(100, .05)

  bf.add(1234)

  println(bf.contains(1234))
}
