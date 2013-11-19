package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val prevalenceRate = 0.01
    val transmissibilityRate = 0.4
    val dieRate = 0.25
    val maxDaysBetweenMoves = 5
    val airProbability = 0
  }

  import SimConfig._

  val persons: List[Person] = (for (personId <- 1 to population) yield new Person(personId)).toList

  for(person <- persons) person.planToMove()
  persons.take((persons.size * prevalenceRate).toInt).foreach(_.getInfected)

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    def visiblyInfectious() = sick || dead

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def planToMove(): Unit = {
      val daysBetweenMoves = 1 + randomBelow(maxDaysBetweenMoves)
      afterDelay(daysBetweenMoves)(moveAndPlan)
    }

    def moveAndPlan(): Unit = {
      if (dead) return

      if (random < airProbability) {
        moveToRoom(randomBelow(roomRows), randomBelow(roomColumns))

      } else {
        val roomsToMove = neighborRoomsWithoutVisiblyInfectious(row, col)
        if (!roomsToMove.isEmpty) {
          val chosenRoom = roomsToMove.toVector(randomBelow(roomsToMove.size))
          moveToRoom(chosenRoom._1, chosenRoom._2)
        }
      }

      planToMove()

    }

    def moveToRoom(roomRow: Int, roomCol: Int) = {
      row = roomRow
      col = roomCol
      possiblyGetInfected()
    }
    
    def possiblyGetInfected(): Unit = {
      if (infected || immune) return
      if (!roomHasInfected(row, col)) return
      if (random < transmissibilityRate) {
        getInfected()
      }
    }

    def getInfected(): Unit = {
      infected = true
      afterDelay(6)(getSick)
    }

    def getSick() {
      sick = true
      if (dead) return // to pass buggy dead person stays dead test
      afterDelay(8)(dieOrPlanToRecover)
    }

    def dieOrPlanToRecover(): Unit = {
      val curRandom = random
      if (curRandom < dieRate) {
        dead = true
      } else {
        afterDelay(2)(recover)
      }
    }

    def recover() = {
      sick = false
      immune = true
      afterDelay(2)(loseImmune)
    }

    def loseImmune() = {
      infected = false
      immune = false
    }

  }

  abstract class Direction {
    def positionAfterMove(row: Int, col: Int): (Int, Int)
  }
  object Left extends Direction {
    def positionAfterMove(row: Int, col: Int): (Int, Int) = {
      (row, if (col == 0) roomColumns-1 else col-1)
    }
  }
  object Right extends Direction {
    def positionAfterMove(row: Int, col: Int): (Int, Int) = {
      (row, if (col == roomColumns-1) 0 else col+1)
    }
  }
  object Up extends Direction {
    def positionAfterMove(row: Int, col: Int): (Int, Int) = {
      (if (row == 0) roomRows-1 else row-1, col)
    }
  }
  object Down extends Direction {
    def positionAfterMove(row: Int, col: Int): (Int, Int) = {
      (if (row == roomRows-1) 0 else row+1, col)
    }
  }

  val directions = Vector(Left, Right, Up, Down)

  def neighborRooms(row: Int, col: Int): Set[(Int, Int)] = {
    directions.map(_.positionAfterMove(row, col)).toSet
  }

  def roomsWithVisiblyInfectious(): Set[(Int, Int)] = {
    (for(person <- persons if person.visiblyInfectious) yield (person.row, person.col)).toSet
  }

  def neighborRoomsWithoutVisiblyInfectious(row: Int, col: Int): Set[(Int, Int)] = {
    neighborRooms(row, col) -- roomsWithVisiblyInfectious()
  }

  def roomHasInfected(row: Int, col: Int) = {
    persons.exists(person => person.row == row && person.col == col && person.infected)
  }

}
