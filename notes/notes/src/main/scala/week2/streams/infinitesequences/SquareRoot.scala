package week2.streams.infinitesequences

object SquareRoot extends App {

  val sqrtOf16 = sqrtStream(16)

  println(sqrtOf16.take(100).toList)

  //println(sqrtOf16.filter(isGoodEnough(_, 16)).take(10).toList)

  def sqrtStream(x: Double): Stream[Double] = {
    def improve(guess: Double) = (guess + x / guess) / 2
    lazy val guesses: Stream[Double] = 1 #:: guesses.map(improve)
    guesses
  }

  def isGoodEnough(guess: Double, x: Double) =
    math.abs((guess * guess - 1) / x) < 0.0001
}
