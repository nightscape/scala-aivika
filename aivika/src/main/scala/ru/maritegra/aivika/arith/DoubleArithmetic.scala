/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class DoubleArithmetic extends UnaryArithmetic[Double, Double] {

  def plus(x: Dynamics[Double]) = x

  def minus(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = - x.apply(p)

  }
}
