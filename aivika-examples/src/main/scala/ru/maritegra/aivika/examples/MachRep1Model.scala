/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

/*
There are two machines, which sometimes break down. Up time is exponentially
distributed with mean 1.0, and repair time is exponentially distributed with
mean 0.5. There are two repairpersons, so the two machines can be repaired
simultaneously if they are down at the same time. Output is long-run
proportion of up time. Should get value of about 0.66.
 */

package ru.maritegra.aivika.examples

import ru.maritegra.aivika._
import ru.maritegra.aivika.random._
import ru.maritegra.aivika.SystemDynamics._

object MachRep1Model {

  val upRate = 1.0 / 1.0      // reciprocal of mean up time
  val repairRate = 1.0 / 0.5  // reciprocal of mean repair time

  val rnd = new Parameter(() => new ExpRandom)

  val queue = new EventQueue

  val totalUpTime = new Ref(queue, () => 0.0)

  val upTimeProportion = totalUpTime / (2 * time)

  class Machine(queue: EventQueue) extends Process(queue) {

    override protected def activate(): Unit @process = {

      while (true) {

        val startUpTime = P(time)
        val upTime = P(rnd).next(upRate)
        hold(upTime)

        val finishUpTime = P(time)
        P(totalUpTime.inc(finishUpTime - startUpTime))

        val repairTime = P(rnd).next(repairRate)
        hold(repairTime)
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
