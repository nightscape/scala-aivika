/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

/*
Two machines, but sometimes break down. Up time is exponentially distributed
with mean 1.0, and repair time is exponentially distributed with mean 0.5.
In this example, there is only one repair-person, so the two machines cannot
be repaired simultaneously if they are down at the same time.

In addition to finding the long-run proportion of up time, let us also find
the long-run proportion of the time that a given machine has an immediate
access to the repair-person when the machine breaks down. Output values
should be about 0.6 and 0.67.
 */

package ru.maritegra.aivika.examples

import ru.maritegra.aivika._
import ru.maritegra.aivika.random._
import ru.maritegra.aivika.SystemDynamics._

object MachRep2Model {

  val upRate = 1.0 / 1.0      // reciprocal of mean up time
  val repairRate = 1.0 / 0.5  // reciprocal of mean repair time

  val rnd = new Parameter(() => new ExpRandom)

  val queue = new EventQueue()

  // number of times the machines have broken up
  val nRep = new Ref(queue, () => 0)

  // number of breakdowns in which the machine
  // started repair service right away
  val nImmedRep = new Ref(queue, () => 0)

  // total up time for all machines
  val totalUpTime = new Ref(queue, () => 0.0)

  // the limited resource with initial count 1
  val repairPerson = new Resource(queue, initCount = 1)

  // the long-run proportion of up-time
  val upTimeProportion = totalUpTime / (2 * time)

  // the long-run proportion of that time when
  // the broken machine has an immediate access
  // to the repair-person (the repair-person availability)
  val repProportion = (1.0 * nImmedRep) / (1.0 * nRep)

  class Machine(queue: EventQueue) extends Process(queue) {

    protected def activate(): Unit @process = {

      while (true) {

        val startUpTime = P(time)
        val upTime = P(rnd).next(upRate)
        hold(upTime)

        val finishUpTime = P(time)
        P(totalUpTime.inc(finishUpTime - startUpTime))

        // check the resource availability
        P(nRep.inc(1))
        val n = P(repairPerson.count)

        when (n == 1) {
          P(nImmedRep.inc(1))
        }

        repairPerson.request()
        val repairTime = P(rnd).next(repairRate)
        hold(repairTime)
        repairPerson.release()
      }
    }
  }

  val machine1 = new Machine(queue)
  val machine2 = new Machine(queue)

  def activate(p: Point) {

    val t0 = starttime(p)

    machine1.run(t0, p)
    machine2.run(t0, p)
  }
}