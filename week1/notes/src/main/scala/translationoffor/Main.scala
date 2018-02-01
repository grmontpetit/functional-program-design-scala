package translationoffor

object Main extends App {

  val d = (1 until 10).flatMap(i =>
    (1 until 11).withFilter(j => isPrime(i+j))
        .map(j => (i, j))
  )

  print(d)

  def isPrime: Int => Boolean = { i =>
    if (i <= 1) false
    else if (i == 2) true
    else !(2 until i).exists(x => i % x == 0)
  }
}
