/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

object SystemDynamics {

  val time = new Dynamics[Double] {

    def apply(p: Point): Double = p.time
  }

  val starttime = new Dynamics[Double] {

    def apply(p: Point): Double = p.specs.starttime
  }

  val stoptime = new Dynamics[Double] {

    def apply(p: Point): Double = p.specs.stoptime
  }

  val dt = new Dynamics[Double] {

    def apply(p: Point): Double = p.specs.dt
  }

  /**
   * Test whether the current simulation time falls on the integration time point.
   */
  def timeIntegration = new Dynamics[Boolean] {

    def apply(p: Point): Boolean = (p.phase >= 0)
  }

  private def integEuler(y: => Dynamics[Double],
                         f: => Dynamics[Double],
                         i: Dynamics[Double],
                         p: Point): Double = {

    val s = p.specs
    val n = p.iteration
    val ph = p.phase

    ph match {

      case 0 =>

        if (n == 0) {
          i(p)

        } else {

          val ty = s.time(n - 1, 0)
          val t1 = ty

          val py = Point(s, p.run, ty, n - 1, 0)
          val p1 = py

          val vy = y(py)
          val k1 = f(p1)

          vy + s.dt * k1
        }

      case _ =>
	      sys.error("Incorrect phase = " + ph)
    }
  }

  private def integRK2(y: => Dynamics[Double],
                       f: => Dynamics[Double],
                       i: Dynamics[Double],
                       p: Point): Double = {

    val s = p.specs
    val n = p.iteration
    val ph = p.phase

    ph match {

      case 0 =>

        if (n == 0) {
          i(p)

        } else {

          val ty = s.time(n - 1, 0)
          val t1 = ty
          val t2 = s.time(n - 1, 1)

          val py = Point(s, p.run, ty, n - 1, 0)
          val p1 = py
          val p2 = Point(s, p.run, t2, n - 1, 1)

          val vy = y(py)
          val k1 = f(p1)
          val k2 = f(p2)

          vy + s.dt / 2.0 * (k1 + k2)
        }

      case 1 =>

        val ty = s.time(n, 0)
        val t1 = ty

        val py = Point(s, p.run, ty, n, 0)
        val p1 = py

        val vy = y(py)
        val k1 = f(p1)

        vy + s.dt * k1

      case _ =>
	      sys.error("Incorrect phase = " + ph)
    }
  }

  private def integRK4(y: => Dynamics[Double],
                       f: => Dynamics[Double],
                       i: Dynamics[Double],
                       p: Point): Double = {

    val s = p.specs
    val n = p.iteration
    val ph = p.phase

    ph match {

      case 0 =>

        if (n == 0) {
          i(p)

        } else {

          val ty = s.time(n - 1, 0)
          val t1 = ty
          val t2 = s.time(n - 1, 1)
          val t3 = s.time(n - 1, 2)
          val t4 = s.time(n - 1, 3)

          val py = Point(s, p.run, ty, n - 1, 0)
          val p1 = py
          val p2 = Point(s, p.run, t2, n - 1, 1)
          val p3 = Point(s, p.run, t3, n - 1, 2)
          val p4 = Point(s, p.run, t4, n - 1, 3)

          val vy = y(py)
          val k1 = f(p1)
          val k2 = f(p2)
          val k3 = f(p3)
          val k4 = f(p4)

          vy + s.dt / 6.0 * (k1 + 2.0 * k2 + 2.0 * k3 + k4)
        }

      case 1 =>

        val ty = s.time(n, 0)
        val t1 = ty

        val py = Point(s, p.run, ty, n, 0)
        val p1 = py

        val vy = y(py)
        val k1 = f(p1)

        vy + s.dt / 2.0 * k1

      case 2 =>

        val ty = s.time(n, 0)
        val t2 = s.time(n, 1)

        val py = Point(s, p.run, ty, n, 0)
        val p2 = Point(s, p.run, t2, n, 1)

        val vy = y(py)
        val k2 = f(p2)

        vy + s.dt / 2.0 * k2

      case 3 =>

        val ty = s.time(n, 0)
        val t3 = s.time(n, 2)

        val py = Point(s, p.run, ty, n, 0)
        val p3 = Point(s, p.run, t3, n, 2)

        val vy = y(py)
        val k3 = f(p3)

        vy + s.dt * k3

      case _ =>
	      sys.error("Incorrect phase = " + ph)
    }
  }

  def integ(f: => Dynamics[Double], i: Dynamics[Double]): Dynamics[Double] = {

    object o {

      lazy val y = Memo.memoDouble(z)

      val z: Dynamics[Double] = new Dynamics[Double] {

        def apply(p: Point): Double = p.specs.method match {

          case Euler => integEuler(y, f, i, p)
          case RungeKutta2 => integRK2(y, f, i, p)
          case RungeKutta4 => integRK4(y, f, i, p)
        }
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
