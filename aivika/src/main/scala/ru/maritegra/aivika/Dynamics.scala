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
   * Returns a value of the process at the specified time point.
   */
  def applyForDouble(p: Point): Double = apply(p).asInstanceOf[Double]

  /**
   * Returns a value of the process at the specified time point.
   */
  def applyForFloat(p: Point): Float = apply(p).asInstanceOf[Float]

  /**
   * Returns a value of the process at the specified time point.
   */
  def applyForLong(p: Point): Long = apply(p).asInstanceOf[Long]

  /**
   * Returns a value of the process at the specified time point.
   */
  def applyForInt(p: Point): Int = apply(p).asInstanceOf[Int]

  /**
   * Returns a value of the process at the specified time point.
   */
  def applyForShort(p: Point): Short = apply(p).asInstanceOf[Short]

  /**
   * Returns a value of the process at the specified time point.
   */
  def applyForByte(p: Point): Byte = apply(p).asInstanceOf[Byte]

  /**
   * Returns a value of the process at the specified time point.
   */
  def applyForBoolean(p: Point): Boolean = apply(p).asInstanceOf[Boolean]

  /**
   * Returns a value of the process at the specified time point.
   */
  def applyForChar(p: Point): Char = apply(p).asInstanceOf[Char]

  /**
   * The monadic `bind` function.
   */
  def flatMap[B](cont: A => Dynamics[B]): Dynamics[B] = new Dynamics[B] {

    def apply(p: Point): B = {

      cont(outer(p))(p)
    }
  }

  /**
   * The mapping function.
   */
  def map[B](f: A => B): Dynamics[B] = new Dynamics[B] {

    def apply(p: Point): B = {

      f(outer(p))
    }
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
  def apply[A](a: A): Dynamics[A] = new Dynamics[A] {

    def apply(p: Point): A = a
  }

  /**
   * The monadic `return` function.
   */
  def apply(a: Double): Dynamics[Double] = new Dynamics[Double] {

    def apply(p: Point): Double = a

    override def applyForDouble(p: Point): Double = a
  }

  /**
   * The monadic `return` function.
   */
  def apply(a: Float): Dynamics[Float] = new Dynamics[Float] {

    def apply(p: Point): Float = a

    override def applyForDouble(p: Point): Double = a
    override def applyForFloat(p: Point): Float = a
  }

  /**
   * The monadic `return` function.
   */
  def apply(a: Long): Dynamics[Long] = new Dynamics[Long] {

    def apply(p: Point): Long = a

    override def applyForDouble(p: Point): Double = a
    override def applyForFloat(p: Point): Float = a
    override def applyForLong(p: Point): Long = a
  }

  /**
   * The monadic `return` function.
   */
  def apply(a: Int): Dynamics[Int] = new Dynamics[Int] {

    def apply(p: Point): Int = a

    override def applyForDouble(p: Point): Double = a
    override def applyForFloat(p: Point): Float = a
    override def applyForLong(p: Point): Long = a
    override def applyForInt(p: Point): Int = a
  }

  /**
   * The monadic `return` function.
   */
  def apply(a: Short): Dynamics[Short] = new Dynamics[Short] {

    def apply(p: Point): Short = a

    override def applyForDouble(p: Point): Double = a
    override def applyForFloat(p: Point): Float = a
    override def applyForLong(p: Point): Long = a
    override def applyForInt(p: Point): Int = a
    override def applyForShort(p: Point): Short = a
  }

  /**
   * The monadic `return` function.
   */
  def apply(a: Byte): Dynamics[Byte] = new Dynamics[Byte] {

    def apply(p: Point): Byte = a

    override def applyForDouble(p: Point): Double = a
    override def applyForFloat(p: Point): Float = a
    override def applyForLong(p: Point): Long = a
    override def applyForInt(p: Point): Int = a
    override def applyForShort(p: Point): Short = a
    override def applyForByte(p: Point): Byte = a
  }

  /**
   * The monadic `return` function.
   */
  def apply(a: Boolean): Dynamics[Boolean] = new Dynamics[Boolean] {

    def apply(p: Point): Boolean = a

    override def applyForBoolean(p: Point): Boolean = a
  }

  /**
   * The monadic `return` function.
   */
  def apply(a: Char): Dynamics[Char] = new Dynamics[Char] {

    def apply(p: Point): Char = a

    override def applyForChar(p: Point): Char = a
  }

  /**
   * Creates a process from the sequence.
   */
  def fromSeq[A](xs: Dynamics[A]*) = new Dynamics[Seq[A]] {

    def apply(p: Point): Seq[A] = xs.map(_.apply(p))
  }

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

        ts.add(t2, x.applyForDouble(p2))
      }

      ts
    }
  }
}
