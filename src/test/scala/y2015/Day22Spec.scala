package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day22Spec extends AnyFlatSpec with Matchers with Day22 {
  "Basics" should "be working soon" in {
    val player = Player("me", hit = 50, available = 500)
    val boss = Player("boss", hit = 58, damage = 9)
    val game = Game(player, boss)
    val games: Seq[Seq[Game]] = game.playAll
    games.forall(game => game.head.isOver) shouldBe true
    //println(games.count(_.head.playerWins))
    //println(games.count(!_.head.playerWins))
    //games.foreach(g => println(g.head))
    //println(games.filter(_.head.playerWins).minBy(_.head.player.spent).head.player.spent)
  }
}

