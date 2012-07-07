/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

import ru.maritegra.aivika.statistics._
import ru.maritegra.aivika.util._

/**
 * Like the `Ref` class it represents a stateful object bound with the event queue but that keeps the
 * history of changes, which allows using such variables in the differential equations, for example. Also it allows
 * gathering accurate statistics with help of the `Var.statistics` method.
 *
 * Binding with the event queue means that each time you request for a variable value or update it at the specified
 * simulation time, the variable checks the event queue forcing the latter to actuate all pending events. It allows
 * keeping the state of the variable actual and coordinated with the rest part of the model, where the event queue
 * plays a role of the coordination center.
 *
 * You can update the variable state with help of methods `write` and `modify`. Also you can take a snap of
 * the history changes using the `freeze` method. Finally, you can gather statistics using the mentioned
 * `Var.statistics` method.
 */
class Var[A](val queue: EventQueue, init: Int => A, val overriding: Boolean)(implicit m: Manifest[A])
  extends Dynamics[A] with Stateful {

  def this(queue: EventQueue, init: Int => A)(implicit m: Manifest[A]) =
    this(queue, init, true)

  def this(queue: EventQueue, init: () => A, overriding: Boolean)(implicit m: Manifest[A]) =
    this(queue, i => init(), overriding)

  def this(queue: EventQueue, init: () => A)(implicit m: Manifest[A]) =
    this(queue, i => init(), true)

  private def initSynchronously(run: Run): A = run.simulation match {

    case Some(x) => x.synchronized { init(run.index) }
    case _ => init(run.index)
  }

  private class State(var ts: ResizeDoubleArray, var xs: ResizeArray[A])

  private val state = new RunMemo((run: Run) => {

    val ts = new ResizeDoubleArray
    val xs = new ResizeArray[A]

    ts += run.specs.starttime
    xs += initSynchronously(run)

    new State(ts, xs)
  })

  /**
   * Request for the variable value at the specified time point.
   */
  def apply(p: Point): A = {

    queue.run(p)

    val s = state(p.run)

    val ts = s.ts
    val xs = s.xs

    val count = ts.length
    val index = count - 1

    val t = ts(index)

    if (t <= p.time) {
      xs(index)

    } else {

      val index = ts.binarySearch(p.time, !overriding)
      xs(if (index >= 0) index else (~index - 1))
    }
  }

  /**
   * Return a computation that can write the new value for the variable at the desired time point.
   *
   * The old values are stored as well. They can be requested using a less time point.
   * It makes safe using the variables in the differential equations, for example.
   */
  def write(a: A): Dynamics[Unit] = new Dynamics[Unit] {
    def apply(p: Point): Unit = write(a, p)
  }

  /**
   * Write the new value for the variable at the specified time point.
   *
   * The old values are stored as well. They can be requested using a less time point.
   * It makes safe using the variables in the differential equations, for example.
   */
  def write(a: A, p: Point) {

    queue.run(p)

    val s = state(p.run)

    val ts = s.ts
    val xs = s.xs

    val count = ts.length
    val index = count - 1

    val t = ts(index)

    if (p.time < t) {
      throw new IllegalStateException("Cannot update the past data.")
      
    } else if (overriding && (p.time == t)) {
      
      xs(index) = a

      processChanged(p)

    } else {

      ts += p.time
      xs += a

      processChanged(p)
    }
  }

  /**
   * Return a computation that can update the variable value according the specified transformation
   * at the desired time point.
   *
   * The old values are stored as well. They can be requested using a less time point.
   * It makes safe using the variables in the differential equations, for example.
   */
  def modify(f: A => A): Dynamics[Unit] = new Dynamics[Unit] {
    def apply(p: Point): Unit = modify(f, p)
  }

  /**
   * Update the variable value according the specified transformation at the given time point.
   *
   * The old values are stored as well. They can be requested using a less time point.
   * It makes safe using the variables in the differential equations, for example.
   */
  def modify(f: A => A, p: Point) {

    queue.run(p)

    val s = state(p.run)

    val ts = s.ts
    val xs = s.xs

    val count = ts.length
    val index = count - 1

    val t = ts(index)

    if (p.time < t) {
      throw new IllegalStateException("Cannot update the past data.")
      
    } else if (overriding && (p.time == t)) {
      
      xs(index) = f(xs(index))

      processChanged(p)

    } else {

      ts += p.time
      xs += f(xs(index))

      processChanged(p)
    }
  }

  private def processChanged(p: Point) {
    changedSource.trigger(this, p)
  }

  /**
   * Return a computation that can take a snap of the variable history as two arrays: the ordered time points and
   * the corresponded variable values.
   *
   * Calling the returned computation at the simulation point is a time consuming operation,
   * for the history of changes is copied into new arrays.
   */
  def freeze: Dynamics[(Array[Double], Array[A])] = new Dynamics[(Array[Double], Array[A])] {

    def apply(p: Point): (Array[Double], Array[A]) = {

      queue.run(p)

      val s = state(p.run)

      val ts = s.ts
      val xs = s.xs

      (ts.toArray, xs.toArray)
    }
  }

  /**
   * Return a computation that increases the variable value.
   */
  def inc(a: A)(implicit n: Numeric[A]): Dynamics[Unit] = modify(n.plus(_, a))

  /**
   * Increase the variable value at the specified time point.
   */
  def inc(a: A, p: Point)(implicit n: Numeric[A]): Unit = modify(n.plus(_, a), p)

  /**
   * Return a computation that decreases the variable value.
   */
  def dec(a: A)(implicit n: Numeric[A]): Dynamics[Unit] = modify(n.minus(_, a))

  /**
   * Decrease the variable value at the specified time point.
   */
  def dec(a: A, p: Point)(implicit n: Numeric[A]): Unit = modify(n.minus(_, a), p)
}

object Var {

  /**
   * Returns the time statistics for the specified variable.
   *
   * Unlike the `Dynamics.statistics` method it gathers statistics in all time points when the variable had changed.
   * It is possible due to the fact that the variable keeps all the history of changes.
   *
   * This method returns a time consuming computation. A usual scenario is that when you request for the statistics
   * in the final time point.
   */
  def statistics(x: Var[Double]): Dynamics[TimeStatistics] = new Dynamics[TimeStatistics] {

    def apply(p: Point): TimeStatistics = {

      x.queue.run(p)

      val s = x.state(p.run)

      val ts = s.ts
      val xs = s.xs

      val count = ts.length
      val index = count - 1

      val t = ts(index)

      val i1 = if (t <= p.time) index else ts.binarySearch(p.time, !x.overriding)
      val i2 = if (i1 >= 0) i1 else (~i1 - 1)

      val its = new IrregularTimeStatistics

      for (i <- 0 to i2) {
        its.add(ts(i), xs(i))
      }

      its
    }
  }

  /**
   * Returns the time statistics for the specified variable.
   *
   * Unlike the `Dynamics.statistics` method it gathers statistics in all time points when the variable had changed.
   * It is possible due to the fact that the variable keeps all the history of changes.
   *
   * This method returns a time consuming computation. A usual scenario is that when you request for the statistics
   * in the final time point.
   */
  def statistics[A](x: Var[A])(implicit n: Numeric[A]): Dynamics[TimeStatistics] = new Dynamics[TimeStatistics] {

    def apply(p: Point): TimeStatistics = {

      x.queue.run(p)

      val s = x.state(p.run)

      val ts = s.ts
      val xs = s.xs

      val count = ts.length
      val index = count - 1

      val t = ts(index)

      val i1 = if (t <= p.time) index else ts.binarySearch(p.time, !x.overriding)
      val i2 = if (i1 >= 0) i1 else (~i1 - 1)

      val its = new IrregularTimeStatistics

      for (i <- 0 to i2) {
        its.add(ts(i), n.toDouble(xs(i)))
      }

      its
    }
  }
}
