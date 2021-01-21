package qlearning

import scala.util.Random

object Main extends App {
  val actions: Seq[Action] = Seq(
    Action("e", "f"),
    Action("e", "l"),
    Action("f", "t"),
    Action("f", "e"),
    Action("t", "e"),
    Action("l", "e"),
    Action("l", "b"),
    Action("b", "e")
  )
  val scoreMap = actions.map(v => (v, 0.0)).toMap ++ Map(Action("f", "t") -> 10.0, Action("l", "b") -> -10.0)
  new QLearningMachine(actions, scoreMap).solve("e")
}

class QLearningMachine(
    actions: Seq[Action],
    scoreMap: Map[Action, Double]
) {
  val rate = 0.1
  val epsilon = 0.3
  val gamma = 0.9
  private val map =
    scala.collection.mutable.Map[Action, Double](actions.map(v => (v, 0.0)): _*)
  def solve(start: String, max: Int = 10000) {
    println(map.map(v => v._1.from + "->" + v._1.to).mkString(","))
    var status = start
    for (_ <- 0 to max) {
      val action = selectAction(status)
      val newScore = calcNewScore(action)
      map.put(action, newScore)
      status = action.to
      println(map.map(v => f"${v._2}%2.2f").mkString(","))
    }
  }

  private def calcNewScore(action: Action): Double = {
    val cand = actions.filter(v => v.from == action.to)
    val max = cand.map(v => map.get(v).get).sortWith((v, l) => v > l).head
    (1 - rate) * map.get(action).get + rate * (scoreMap.get(action).get + gamma * max)
  }

  private def selectAction(from: String): Action = {
    val candi = actions.filter(v => v.from == from)
    if (Random.nextDouble() < epsilon) {
      candi(Random.nextInt(candi.size))
    } else {
      candi.sortWith((l, r) => map.get(l).get > map.get(r).get).head
    }
  }
}

case class Action(from: String, to: String)
