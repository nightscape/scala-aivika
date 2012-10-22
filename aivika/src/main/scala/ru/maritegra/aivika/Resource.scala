/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

import scala.collection.mutable.Queue
import scala.util.continuations._

class Resource(val queue: EventQueue, val initCount: Int, val createCount: Int) {

  def this(queue: EventQueue, initCount: Int) = this(queue, initCount, initCount)

  private class State(var count: Int, var waitQueue: Queue[Unit => Dynamics[Unit]])

  private val state = new RunMemo((r: Run) => new State(createCount, new Queue))

  val count: Dynamics[Int] = new Dynamics[Int] {

    def apply(p: Point): Int = {

      val s = state(p.run)

      s.count
    }
  }

  def request(): Unit @process = shift {

    (k : Unit => Dynamics[Unit]) => new Dynamics[Unit] {

      def apply(p: Point): Unit = {

        val s = state(p.run)

        if (s.count == 0) {
          s.waitQueue += k

        } else {

          s.count -= 1
          k()(p)
        }
      }
    }
  }

  def release(): Unit @process = shift {

    (k: Unit => Dynamics[Unit]) => new Dynamics[Unit] {

      def apply(p: Point): Unit = {

        val s = state(p.run)

        if (s.count + 1 > initCount) {
          sys.error("The resource count cannot be greater than its intial value.")

        } else {

          if (s.waitQueue.isEmpty) {
            s.count += 1

          } else {

            val c2 = s.waitQueue.dequeue()
            queue.enqueueCont(p.time, c2, p)
          }

          k()(p)
        }
      }
    }
  }
}
