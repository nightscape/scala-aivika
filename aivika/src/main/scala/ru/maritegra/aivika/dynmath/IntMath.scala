/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.dynmath

import ru.maritegra.aivika._

abstract class IntMath extends NumMath[Int] {

  def abs(x: Dynamics[Int]) = new Dynamics[Int] {

    def apply(p: Point): Int =
      math.abs(x.apply(p))

  }

  def max(x: Dynamics[Int], y: Dynamics[Int]) = new Dynamics[Int] {

    def apply(p: Point): Int =
      math.max(x.apply(p), y.apply(p))

  }

  def min(x: Dynamics[Int], y: Dynamics[Int]) = new Dynamics[Int] {

    def apply(p: Point): Int =
      math.min(x.apply(p), y.apply(p))

  }

  def signum(x: Dynamics[Int]) = new Dynamics[Int] {

    def apply(p: Point): Int =
      math.signum(x.apply(p))

  }
}
