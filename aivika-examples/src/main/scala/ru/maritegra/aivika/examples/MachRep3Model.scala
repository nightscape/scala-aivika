/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

/*
Variation of models MachRep1Model, MachRep2Model. Two machines, but
sometimes break down. Up time is exponentially distributed with mean
1.0, and repair time is exponentially distributed with mean 0.5. In
this example, there is only one repair person, and she is not summoned
until both machines are down. We find the proportion of up time. It
should come out to about 0.45.
 */

package ru.maritegra.aivika.examples

import ru.maritegra.aivika._
import ru.maritegra.aivika.random._
import ru.maritegra.aivika.SystemDynamics._

object MachRep3Model {

  val upRate = 1.0 / 1.0      // reciprocal of mean up time
  val repairRate = 1.0 / 0.5  // reciprocal of mean repair time

  val rnd = new Parameter(() => new ExpRandom)

  val queue = new EventQueue

  // number of machines currently up
  val nUp = new Ref(queue, () => 2)

  // total up time for all machines
  val totalUpTime = new Ref(queue, () => 0.0)

  // the limited resource
  val repairPerson = new Resource(queue, initCount = 1)

  // the long-run proportion of up-time
  val upTimeProportion = totalUpTime / (2 * time)

  class Machine(queue: EventQueue, other: => Machine) extends Process(queue) {

    protected def activate(): Unit @process = {

      while (true) {

        val startUpTime = P(time)
        val upTime = P(rnd).next(upRate)
        hold(upTime)

        val finishUpTime = P(time)
        P(totalUpTime.inc(finishUpTime - startUpTime))

        P(nUp.dec(1))
        if (P(nUp) == 1) {
          passivate()

        } else {

          val n = P(repairPerson.count)
          when (n == 1) { P(other.reactivate) }
        }

        repairPerson.request()
        val repairTime = P(rnd).next(repairRate)
        hold(repairTime)
        P(nUp.inc(1))
        repairPerson.release()
      }
    }
  }

  lazy val machine1: Machine = new Machine(queue, machine2)
  lazy val machine2: Machine = new Machine(queue, machine1)

  def activate(p: Point) {

    val t0 = starttime(p)

    machine1.run(t0, p)
    machine2.run(t0, p)
  }
}