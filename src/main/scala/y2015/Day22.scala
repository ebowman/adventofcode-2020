package y2015

trait Day22 {

  case class Game(player: Player, boss: Player, time: Int = 0) {
    override def toString: String = s"Game(time=$time,$player,$boss"

    def playerWins: Boolean = boss.dead

    def isOver: Boolean = boss.dead || player.dead

    def playAll: Seq[Seq[Game]] = {
      def recurse(gs: Seq[Game]): Seq[Seq[Game]] = {
        (for {
          g <- gs
          n <- g.next
        } yield {
          if (n.isOver) {
            Seq(Seq(g, n).reverse)
          }
          else {
            recurse(Seq(n)).map { s => s :+ g }
          }
        }).flatten
      }

      recurse(Seq(this))
    }

    def next: Seq[Game] = {
      val players = player.mananize(time)
      if (players.isEmpty) Seq(Game(player.copy(outOfMoney = true), boss, time + 1))
      else {
        for {
          player <- players
        } yield {
          if ((time % 2) == 0) { // player turn
            // play the effects of existing mana
            val (p1, b1) = player.mana.foldLeft((player, boss)) {
              case ((player, boss), mana) => mana.playerTurn(player, boss)
            }

            // turn on any new mana
            val p2 = p1.newMana.map { nM =>
              val p3 = nM.turnOn(player)
              p3.copy(mana = p3.mana :+ nM, newMana = None)
            }.getOrElse(p1)

            /* no weapons for wizards, it's all spells
            val delta1 = p2.damage - b1.armor
            val b2 = if (b1.hit == boss.hit || delta1 >= 1) {
              b1.copy(hit = b1.hit - Math.max(delta1, 1))
            } else b1
             */

            val expired = p2.mana.filter(_.expired(time + 1))

            // remove expired
            val p4 = p2.copy(mana = p2.mana.diff(expired))

            // turn off expired
            val p5 = expired.foldLeft(p4) {
              case (p, m) => m.turnOff(p)
            }
            Game(p5, b1, time + 1)
          } else { // boss turn

            // play the effects of existing mana
            val (p1, b1) = player.mana.foldLeft((player, boss)) {
              case ((player, boss), mana) => mana.playerTurn(player, boss)
            }

            val delta1 = b1.damage - p1.armor
            val p2 = if (p1.hit == player.hit || delta1 >= 1) {
              p1.copy(hit = p1.hit - Math.max(delta1, 1))
            } else p1

            val expired = p2.mana.filter(_.expired(time + 1))

            // remove expired
            val p4 = p2.copy(mana = p2.mana.diff(expired))

            // turn off expired
            val p5 = expired.foldLeft(p4) {
              case (p, m) => m.turnOff(p)
            }
            Game(p5, b1, time + 1)
          }
        }
      }
    }
  }

  case class Player(name: String,
                    hit: Int = 100,
                    damage: Int = 0,
                    armor: Int = 0,
                    spent: Int = 0,
                    available: Int = 0,
                    newMana: Option[Mana] = None,
                    mana: Seq[Mana] = Seq.empty,
                    outOfMoney: Boolean = false) {

    def dead: Boolean = hit <= 0 || outOfMoney

    override def toString = s"$name(hit=$hit,damage=$damage,armor=$armor,spent=$spent,available=$available,mana=$mana,next=${newMana.getOrElse("x")})"

    def mananize(time: Int): Seq[Player] = {
      def avail(t: Int): Seq[Mana] = Mana.mana(t).filter(_.cost <= available)

      avail(time).map(m => copy(newMana = Some(m), spent = spent + m.cost, available = available - m.cost))
    }
  }

  trait Mana {

    override def toString(): String = s"$name/$born/$lifespan"

    def name: String = getClass.getName.replaceAll(".*\\$", "")

    def cost: Int

    def born: Int

    def lifespan: Int

    def age(time: Int): Int = time - born

    def expired(time: Int): Boolean = (time - born) > lifespan

    def playerTurn(player: Player, boss: Player): (Player, Player) = (player, boss)

    def bossTurn(player: Player, boss: Player): (Player, Player) = (player, boss)

    def turnOn(player: Player): Player = player

    def turnOff(player: Player): Player = player
  }

  object Mana {

    def mana(time: Int): Seq[Mana] = Seq(MagicMissile(time), Drain(time), Shield(time), Poison(time), Recharge(time))

    val minAvailable: Int = MagicMissile(0).cost // cheapest

    case class MagicMissile(born: Int) extends Mana {
      val cost = 53
      val lifespan = 1

      override def playerTurn(player: Player, boss: Player) = (player, boss.copy(hit = boss.hit - 4))
    }

    case class Drain(born: Int) extends Mana {
      val cost = 73
      val lifespan = 1

      override def playerTurn(player: Player, boss: Player) = (player, boss.copy(hit = boss.hit - 2))

      override def bossTurn(player: Player, boss: Player) = (player.copy(hit = player.hit + 2), boss)
    }

    case class Shield(born: Int) extends Mana {
      val cost = 113
      val lifespan = 6

      override def turnOn(player: Player): Player = player.copy(armor = player.armor + 7)

      override def turnOff(player: Player): Player = player.copy(armor = player.armor - 7)
    }

    case class Poison(born: Int) extends Mana {
      val cost = 173
      val lifespan = 6

      override def playerTurn(player: Player, boss: Player) = (player, boss.copy(hit = boss.hit - 3))
    }

    case class Recharge(born: Int) extends Mana {
      val cost = 229
      val lifespan = 5

      override def playerTurn(player: Player, boss: Player) = (player.copy(available = player.available + 101), boss)
    }

  }

}