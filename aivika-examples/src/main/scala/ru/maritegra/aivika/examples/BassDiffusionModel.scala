/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.examples

import scala.collection.mutable.Buffer

import ru.maritegra.aivika._
import ru.maritegra.aivika.random._

class BassDiffusionModel {

  val n = 500

  val advertisingEffectiveness: Dynamics[Double] = 0.011
  val contactRate: Dynamics[Double] = 100
  val adoptionFraction: Dynamics[Double] = 0.015

  val rnd = new Parameter(() => new UniformRandom)
  val expRnd = new Parameter(() => new ExpRandom)

  val queue = new EventQueue

  val potentialAdopters = new Ref(queue, () => 0)
  val adopters = new Ref(queue, () => 0)
  val agents = new Ref(queue, () => Buffer[Person]())

  class Person(queue: EventQueue) extends Agent(queue) {

    val potentialAdopter = new AgentState(this) {

      protected override val activate: Dynamics[Unit] =
        Dynamics.fromFunction((p: Point) => {

          potentialAdopters.inc(1, p)

          // create a timeout that will hold 
          // while the state is still active
          
          val delta = expRnd(p).next(advertisingEffectiveness(p))

          addTimeout(delta, agent.activateState(adopter), p)
        })

      protected override val deactivate: Dynamics[Unit] =
        Dynamics.fromFunction((p: Point) => {

          potentialAdopters.dec(1, p)
        })
    }

    val adopter = new AgentState(this) {
      
      protected override val activate: Dynamics[Unit] =
        Dynamics.fromFunction((p: Point) => {
          
          adopters.inc(1, p)
          
          // create a timer that will be repeatedly 
          // actuated while the state is still active
          
          val delta = Dynamics.fromFunction((p: Point) =>
            expRnd(p).next(contactRate(p)))

          val action = Dynamics.fromFunction((p: Point) =>
            sendMessageBuy(p))

          addTimer(delta, action, p)
        })

      protected override val deactivate: Dynamics[Unit] =
        Dynamics.fromFunction((p: Point) => {

          adopters.dec(1, p)
        })
    }

    private def sendMessageBuy(p: Point) {

      // send message `buy` to random agent

      val xs = agents(p)
      val idx = ((xs.length - 1) * rnd(p).next()).round.toInt

      xs(idx).buy(p)
    }

    def buy(p: Point) {

      // accept message `buy`

      state(p) match {

        case Some(st) if (st == potentialAdopter) =>

          if (rnd(p).next() <= adoptionFraction(p)) {
            activateState(adopter, p)
          }

        case _ =>
      }
    }
  }

  def activate(p: Point) {

    // activate the model

    for (i <- 1 to n) {

      val agent = new Person(queue)

      agents(p) += agent
      agent.activateState(agent.potentialAdopter, p)
    }
  }
}

object BassDiffusionModel extends BassDiffusionModel
