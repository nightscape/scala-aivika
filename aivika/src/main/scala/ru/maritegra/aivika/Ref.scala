/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

class Ref[A](val queue: EventQueue, init: Int => A) extends Dynamics[A] with Stateful {

  def this(queue: EventQueue, init: () => A) = this(queue, i => init())

  private val message = "The time is unmatched. Cannot request or update the past data. " +
    "Use " + classOf[Var[A]] + " that keeps the history of changes in time points."

  private def initSynchronously(run: Run): A = run.simulation match {

    case Some(x) => x.synchronized { init(run.index) }
    case _ => init(run.index)
  }

  private class State(var time: Double, var value: A)

  private val state = new RunMemo((run: Run) => {
    new State(run.specs.starttime, initSynchronously(run))
  })

  def apply(p: Point): A = {

    queue.run(p)

    val s = state(p.run)

    if (p.time < s.time) {
      throw new IllegalStateException(message)

    } else {

      s.time = p.time
      s.value
    }
  }

  def write(a: A): Dynamics[Unit] = new Dynamics[Unit] {
    def apply(p: Point): Unit = write(a, p)
  }

  def write(a: A, p: Point) {

    queue.run(p)

    val s = state(p.run)

    if (p.time < s.time) {
      throw new IllegalStateException(message)

    } else {

      s.time = p.time
      s.value = a

      processChanged(p)
    }
  }

  def modify(f: A => A): Dynamics[Unit] = new Dynamics[Unit] {
    def apply(p: Point): Unit = modify(f, p)
  }

  def modify(f: A => A, p: Point) {

    queue.run(p)

    val s = state(p.run)

    if (p.time < s.time) {
      throw new IllegalStateException(message)

    } else {

      s.time = p.time
      s.value = f(s.value)

      processChanged(p)
    }
  }

  private def processChanged(p: Point) {
    changedSource.trigger(this, p)
  }

  def inc(a: A)(implicit n: Numeric[A]): Dynamics[Unit] = modify(n.plus(_, a))
  def inc(a: A, p: Point)(implicit n: Numeric[A]): Unit = modify(n.plus(_, a), p)

  def dec(a: A)(implicit n: Numeric[A]): Dynamics[Unit] = modify(n.minus(_, a))
  def dec(a: A, p: Point)(implicit n: Numeric[A]): Unit = modify(n.minus(_, a), p)
}
