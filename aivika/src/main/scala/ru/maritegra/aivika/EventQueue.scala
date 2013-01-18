/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

import ru.maritegra.aivika.util._

class EventQueue {

  private class State(val pq: PriorityQueue[Unit => Dynamics[Unit]],
		      var busy: Boolean, var time: Double)

  private val state = new RunMemo((run: Run) => {

    new State(new PriorityQueue[Unit => Dynamics[Unit]], 
	      false, run.specs.starttime)
  })

  def enqueue(t: Double, h: Dynamics[Unit]): Dynamics[Unit] = {

    val c = (unit: Unit) => h
    enqueueCont(t, c)
  }

  def enqueue(t: Double, h: Dynamics[Unit], p: Point) {

    val c = (unit: Unit) => h
    enqueueCont(t, c, p)
  }

  def enqueueCont(t: Double, c: Unit => Dynamics[Unit]) = new Dynamics[Unit] {

    def apply(p: Point): Unit = {

      val s = state(p.run)
      val pq = s.pq

      pq.enqueue(t, c)
      processChanged(p)
    }
  }

  def enqueueCont(t: Double, c: Unit => Dynamics[Unit], p: Point) {

    val s = state(p.run)
    val pq = s.pq

    pq.enqueue(t, c)
    processChanged(p)
  }

  val run = new Dynamics[Unit] {

    def apply(p: Point): Unit = {

      val s = state(p.run)

      if (! s.busy) {

        s.busy = true

        try {
          actuate(p, s)

        } finally {
          s.busy = false
        }
      }
    }
  }

  private def actuate(p: Point, s: State) {

    val pq = s.pq

    var f = true

    while (f && !pq.isEmpty) {

      val t2 = pq.frontKey
      val c2 = pq.frontValue

      if (t2 < s.time) {
	      sys.error("The time value is too small.")

      } else if (t2 <= p.time) {

        s.time = t2

        pq.dequeue()

        val n2  = (math.floor((t2 - p.specs.starttime) / p.specs.dt)).toInt
        val ph2 = -1
        val p2  = Point(p.specs, p.run, t2, n2, ph2)

        processChanged(p2)
        c2()(p2)

      } else {
        f = false
      }
    }
  }

  private def processChanged(p: Point) {
    count.changedSource.trigger(this, p)
  }

  val count = new Dynamics[Int] with Stateful {

    def apply(p: Point): Int = {

      EventQueue.this.run(p)

      val s = state(p.run)

      if (p.time < s.time) {

        throw new IllegalStateException("Cannot request for past data. This is " +
          "a lightweight counter which doesn't keep the history of changes, although " +
          "in case of need you may wrap it in the Var variable listening to all changes " +
          "of the counter and updating accordingly the corresponded variable.")

      } else {
        s.pq.length
      }
    }
  }
}
