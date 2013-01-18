/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

abstract sealed class Method {
  def integ(y: => Dynamics[Double],
    f: => Dynamics[Double],
    i: Dynamics[Double],
    p: Point): Double
}

case object Euler extends Method {
  def integ(y: => Dynamics[Double],
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

}
case object RungeKutta2 extends Method {
  def integ(y: => Dynamics[Double],
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

}
case object RungeKutta4 extends Method {

  def integ(y: => Dynamics[Double],
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

}
