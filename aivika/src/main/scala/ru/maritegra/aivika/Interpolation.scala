/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

object Interpolation {

  def discrete[T](x: Dynamics[T]): Dynamics[T] = new Dynamics[T] {

    def apply(p: Point): T = {

      if (p.phase >= 0) x.apply(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.apply(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }
 }

  def discrete0[T](x: Dynamics[T]): Dynamics[T] = new Dynamics[T] {

    def apply(p: Point): T = {

      if (p.phase == 0) x.apply(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.apply(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }
  }
}
