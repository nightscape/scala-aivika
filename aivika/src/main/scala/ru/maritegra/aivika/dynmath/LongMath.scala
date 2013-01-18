/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.dynmath

import ru.maritegra.aivika._

abstract class LongMath extends NumMath[Long] {

  def abs(x: Dynamics[Long]) = new Dynamics[Long] {

    def apply(p: Point): Long =
      math.abs(x.apply(p))

  }

  def max(x: Dynamics[Long], y: Dynamics[Long]) = new Dynamics[Long] {

    def apply(p: Point): Long =
      math.max(x.apply(p), y.apply(p))

  }

  def min(x: Dynamics[Long], y: Dynamics[Long]) = new Dynamics[Long] {

    def apply(p: Point): Long =
      math.min(x.apply(p), y.apply(p))

  }

  def signum(x: Dynamics[Long]) = new Dynamics[Long] {

    def apply(p: Point): Long =
      math.signum(x.apply(p))

  }
}
