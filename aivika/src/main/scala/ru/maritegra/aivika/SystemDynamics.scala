/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

object SystemDynamics {

  val time = Dynamics.fromFunction(_.time)

  val starttime = Dynamics.fromFunction(_.specs.starttime)

  val stoptime = Dynamics.fromFunction(_.specs.stoptime)

  val dt = Dynamics.fromFunction(_.specs.dt)

  /**
   * Test whether the current simulation time falls on the integration time point.
   */
  def timeIntegration = Dynamics.fromFunction(_.phase >= 0)

  def integ(f: => Dynamics[Double], i: Dynamics[Double]): Dynamics[Double] = {

    object o {

      lazy val y = Memo.memoDouble(z)

      val z: Dynamics[Double] = new Dynamics[Double] {

        def apply(p: Point): Double = p.specs.method.integ(y, f, i, p)
      }
    }

    o.y
  }

  /**
   * Return the Net Present Value (NPV).
   */
  def npv(stream: Dynamics[Double],
          discountRate: => Dynamics[Double],
          init: Dynamics[Double],
          factor: Dynamics[Double]): Dynamics[Double] = {

    lazy val df: Dynamics[Double] = integ(- df * discountRate, 1)
    val accum = integ(stream * df, init)

    (accum + dt * stream * df) * factor
  }

  /**
   * Return the Net Present Value End of period (NPVE).
   */
  def npve(stream: Dynamics[Double],
           discountRate: => Dynamics[Double],
           init: Dynamics[Double],
           factor: Dynamics[Double]): Dynamics[Double] = {

    lazy val df: Dynamics[Double] = integ(- df * discountRate / (1 + discountRate * dt), 1 / (1 + discountRate * dt))
    val accum = integ(stream * df, init)

    (accum + dt * stream * df) * factor
  }
}
