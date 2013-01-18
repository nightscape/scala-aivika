/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.dynmath

import ru.maritegra.aivika._

abstract class FloatMath extends RealMath[Float] {

  def abs(x: Dynamics[Float]) = new Dynamics[Float] {

    def apply(p: Point): Float =
      math.abs(x.apply(p))

  }

  def max(x: Dynamics[Float], y: Dynamics[Float]) = new Dynamics[Float] {

    def apply(p: Point): Float =
      math.max(x.apply(p), y.apply(p))

  }

  def min(x: Dynamics[Float], y: Dynamics[Float]) = new Dynamics[Float] {

    def apply(p: Point): Float =
      math.min(x.apply(p), y.apply(p))

  }

  def signum(x: Dynamics[Float]) = new Dynamics[Float] {

    def apply(p: Point): Float =
      math.signum(x.apply(p))

  }

  def round(x: Dynamics[Float]) = new Dynamics[Float] {

    def apply(p: Point): Float =
      math.round(x.apply(p))

  }

  def ulp(x: Dynamics[Float]) = new Dynamics[Float] {

    def apply(p: Point): Float =
      math.ulp(x.apply(p))

  }
}
