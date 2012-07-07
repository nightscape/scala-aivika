/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class FloatArithmetic extends UnaryArithmetic[Float, Float] {

  def plus(x: Dynamics[Float]) = x

  def minus(x: Dynamics[Float]) = new Dynamics[Float] {

    def apply(p: Point): Float = - x.applyForFloat(p)

    override def applyForDouble(p: Point): Double = - x.applyForFloat(p)
    override def applyForFloat(p: Point): Float = - x.applyForFloat(p)
  }
}
