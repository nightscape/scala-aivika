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

object MachRep1EventDrivenModel {

  val upRate = 1.0 / 1.0      // reciprocal of mean up time
  val repairRate = 1.0 / 0.5  // reciprocal of mean repair time

  val rnd = new Parameter(() => new ExpRandom)

  val queue = new EventQueue

  val totalUpTime = new Ref(queue, () => 0.0)

  val upTimeProportion = totalUpTime / (2 * time)

  lazy val machineBroken = (startUpTime: Double) =>
    Dynamics.fromFunction((p: Point) => {

      val finishUpTime = time(p)
      totalUpTime.inc(finishUpTime - startUpTime, p)
      val repairTime = rnd(p).next(repairRate)

      // enqueue a new event
      val t = finishUpTime + repairTime
      queue.enqueue(t, machineRepaired, p)
    })

  lazy val machineRepaired: Dynamics[Unit] =
    Dynamics.fromFunction((p: Point) => {

      val startUpTime = time(p)
      val upTime = rnd(p).next(upRate)

      // enqueue a new event
      val t = startUpTime + upTime
      queue.enqueue(t, machineBroken(startUpTime), p)
    })

  def activate(p: Point) {

    val t0 = starttime(p)

    // start two machines
    queue.enqueue(t0, machineRepaired, p)
    queue.enqueue(t0, machineRepaired, p)
  }
}
