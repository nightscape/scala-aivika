/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

import scala.util.continuations._

import ru.maritegra.aivika.arith._
import ru.maritegra.aivika.statistics._

/**
 * Specifies the dynamic process which is essentially just a function of time.
 */
abstract class Dynamics[+A] extends (Point => A) {

  outer =>

  /**
   * Returns a value of the process at the specified time point.
   */
  def apply(p: Point): A

  /**
   * The monadic `bind` function.
   */
  def flatMap[B](cont: A => Dynamics[B]): Dynamics[B] = Dynamics.fromFunction { p =>
      cont(outer(p))(p)
  }

  /**
   * The mapping function.
   */
  def map[B](f: A => B): Dynamics[B] = Dynamics.fromFunction{ p =>
      f(outer(p))
  }

  /**
   * Adds two processes.
   */
  def +[A1 >: A, B, C](that: Dynamics[B])(implicit arith: BinaryArithmetic[A1, B, C]): Dynamics[C] =
    arith.add(this, that)

  /**
   * Subtracts two processes.
   */
  def -[A1 >: A, B, C](that: Dynamics[B])(implicit arith: BinaryArithmetic[A1, B, C]): Dynamics[C] =
    arith.sub(this, that)

  /**
   * Multiplies two processes.
   */
  def *[A1 >: A, B, C](that: Dynamics[B])(implicit arith: BinaryArithmetic[A1, B, C]): Dynamics[C] =
    arith.mult(this, that)

  /**
   * Divides two processes.
   */
  def /[A1 >: A, B, C](that: Dynamics[B])(implicit arith: BinaryArithmetic[A1, B, C]): Dynamics[C] =
    arith.div(this, that)

  /**
   * The modulo of two processes.
   */
  def %[A1 >: A, B, C](that: Dynamics[B])(implicit arith: BinaryArithmetic[A1, B, C]): Dynamics[C] =
    arith.rem(this, that)

  /**
   * The unary plus.
   */
  def unary_+[A1 >: A, B](implicit arith: UnaryArithmetic[A1, B]): Dynamics[B] =
    arith.plus(this)

  /**
   * The unary minus.
   */
  def unary_-[A1 >: A, B](implicit arith: UnaryArithmetic[A1, B]): Dynamics[B] =
    arith.minus(this)

  /**
   * Runs the simulation and returns the value at the final time point.
   */
  def run1(specs: Specs): A = {

    val run = new Run(specs)

    try {
      
      val n = specs.iterations - 1
      val p = Point(specs, run, specs.time(n, 0), n, 0)

      apply(p)

    } finally {
      run.dispose()
    }
  }

  /**
   * Runs the simulation and returns the values at all integration time points.
   */
  def run(specs: Specs): Iterator[A] = {

    val run = new Run(specs)

    var n = 0
    var broken = false

    new Iterator[A] {

      def hasNext = !broken && (n < specs.iterations)

      def next(): A = {
	
        val p = Point(specs, run, specs.time(n, 0), n, 0)

        try {

          val a = apply(p)

          n += 1

          if (n == specs.iterations) {
            run.dispose()
          }

          a

        } catch {

          case e: Exception =>

            broken = true
            run.dispose()

            throw e
        }
      }
    }
  }

  /**
   * Converts this to a discontinuous process.
   */
  def toProcess: A @process = shift {

    (k: A => Dynamics[Unit]) => new Dynamics[Unit] {

      def apply(p: Point): Unit = {
       
        val a = outer.apply(p)
        k(a)(p)
      }
    }
  }
}

/**
 * A companion object.
 */
object Dynamics {

  /**
   * The monadic `return` function.
   */
  def apply[A](a: A): Dynamics[A] = Dynamics.fromFunction(p => a)

  /**
   * Creates a process from the sequence.
   */
  def fromSeq[A](xs: Dynamics[A]*) = Dynamics.fromFunction(p => xs.map(_.apply(p)))

  /**
   * Creates a process from the function.
   */
  def fromFunction[A](f: Point => A) = new Dynamics[A] {

    def apply(p: Point): A = f(p)
  }

  /**
   * Returns `zero` computation.
   */
  val zero = new Dynamics[Unit] {

    def apply(p: Point): Unit = {}
  }

  /**
   * Returns the time statistics for the specified dynamic process.
   * Only you should always use more accurate method `Var.statistics` for variables whenever possible.
   *
   * The current method gathers statistics in the integration time points starting from the start time
   * and up to the latest passed integration time point which can coincide with the current simulation time,
   * or can be less if the latter is different.
   *
   * Computing statistics is a time consuming operation. Therefore a usual scenario is that when you request for
   * the statistics in the final simulation time.
   *
   * Regarding the mentioned `Var.statistics` method, it gathers statistics in all time points when the variable
   * state had changed. These time points can differ from the integration time points. Hence that method is more
   * accurate. But only the variables keep the history of their changes.
   */
  def statistics(x: Dynamics[Double]): Dynamics[TimeStatistics] = new Dynamics[TimeStatistics] {

    def apply(p: Point): TimeStatistics = {

      val ts = new RegularTimeStatistics

      for (i <- 0 to p.iteration) {

        val t2 = p.specs.time(i, 0)
        val p2 = Point(p.specs, p.run, t2, i, 0)

        ts.add(t2, x.apply(p2))
      }

      ts
    }
  }

  /**
   *  Create a new process that sequentially iterates the input process.
   */
  def iterate(m: Dynamics[Unit]): Dynamics[Unit] = Memo.memoUnit0(m)
}
