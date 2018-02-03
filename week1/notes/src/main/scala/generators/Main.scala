package generators

object Main extends App {
  val integers = new Generator[Int] {
    val rand = new java.util.Random
    def generate = rand.nextInt()
  }

  val booleans = new Generator[Boolean] {
    def generate = integers.generate > 0
  }

  val pairs = new Generator[(Int, Int)] {
    def generate = (integers.generate, integers.generate)
  }

  def bool: Generator[Boolean] = integers.map(x => x > 0)

  def increment: Generator[Int] = integers.map(x => x + 1)

  def stringify: Generator[String] = integers.map(x => x.toString)

  def doubleify: Generator[Double] = integers.map(x => x.toDouble)

  def functionify: Generator[Int => String] = integers.map(x => x => x.toString)

  val mapOfMap: Generator[Generator[Int]] = integers.map(x => new Generator[Int] {
    override def generate: Int = 1 + x
  })

  val flatMapping: Generator[Int] = mapOfMap.flatMap(x => x)

  def pairs[T, U](t: Generator[T], u: Generator[U]) = for {
    x <- t
    y <- u
  } yield (x, y)

  def single[T](x: T): Generator[T] = new Generator[T] {
    def generate = x
  }

  def choose(lo: Int, hi: Int): Generator[Int] = {
    for (x <- integers) yield lo + x % (hi - lo)
  }

  def oneOf[T](xs: T*): Generator[T] =
    for (idx <- choose(0, xs.length)) yield xs(idx)

  println(oneOf("red", "blue", "yellow").generate)

  println(choose(0, 1000).generate)

  println(single("hello").generate)

  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list

  def emptyLists = single(Nil)

  def nonEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail
}

trait Generator[+T] {
  self =>

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    def generate = f(self.generate).generate
  }

}
