/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

import scala.util.continuations._

/**
 * Represents a discontinuous process that can suspend and then resume.
 */
abstract class Process(val queue: EventQueue) {

  /**
   * Defines how the process is activated.
   */
  protected def activate(): Unit @process

  /**
   * Holds the process for the specified amount of time.
   */
  protected[this] def hold(dt: Double): Unit @process = shift {

    (k: Unit => Dynamics[Unit]) => new Dynamics[Unit] {

      def apply(p: Point): Unit = {
        queue.enqueueCont(p.time + dt, k, p)
      }
    }
  }

  private class State(var started: Boolean, var cont: Option[Unit => Dynamics[Unit]])

  private val state = new RunMemo((run: Run) => new State(false, None))

  /**
   * Passivates the discontinuous process.
   */
  protected[this] def passivate(): Unit @process = shift {

    (k: Unit => Dynamics[Unit]) => new Dynamics[Unit] {

      def apply(p: Point): Unit = {

        val s = state(p.run)

        s.cont match {

          case None => s.cont = Some(k)
          case Some(_) => sys.error("Cannot passivate the same process twice.")
        }
      }
    }
  }

  /**
   * Tests whether the process is passivated.
   */
  lazy val passivated: Dynamics[Boolean] = new Dynamics[Boolean] {

    def apply(p: Point): Boolean = {

      val s = state(p.run)

      s.cont match {

        case None => false
        case Some(_) => true
      }
    }
  }

  /**
   * Reactivates the discontinuous process.
   */
  lazy val reactivate: Dynamics[Unit] = new Dynamics[Unit] {

    def apply(p: Point): Unit = {

      val s = state(p.run)

      s.cont match {

        case None =>
        case Some(cont) =>

          s.cont = None

          queue.enqueueCont(p.time, cont, p)
      }
    }
  }

  /**
   * Runs the process at the specified time.
   */
  def run(t: Double): Dynamics[Unit] = new Dynamics[Unit] {
    def apply(p: Point): Unit = Process.this.run(t, p)
  }

  /**
   * Runs the process at the specified time.
   */
  def run(t: Double, p: Point) {

    val s = state(p.run)

    if (s.started)
      sys.error("The process has been started already.")
    else
      s.started = true

    load(t)(p)
  }

  private def load(t: Double): Dynamics[Unit] = reset {

    shift {
          
      (k: Unit => Dynamics[Unit]) => new Dynamics[Unit] {

        def apply(p: Point): Unit = {
          queue.enqueueCont(t, k, p)
        }
      }
    }

    activate()

    Dynamics(())
  }
}
