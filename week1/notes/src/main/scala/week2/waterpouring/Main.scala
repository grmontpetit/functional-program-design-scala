package week2.waterpouring

object Main extends App {
  val problem = new Pouring(Vector(4, 7, 19))
  //println(problem.moves)

  //println(problem.pathSets.take(3).toList)

  println(problem.solution(17))
}
