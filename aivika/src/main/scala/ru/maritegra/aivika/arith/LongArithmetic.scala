/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class LongArithmetic extends UnaryArithmetic[Long, Long] {

  def plus(x: Dynamics[Long]) = x

  def minus(x: Dynamics[Long]) = new Dynamics[Long] {

    def apply(p: Point): Long = - x.applyForLong(p)

    override def applyForDouble(p: Point): Double = - x.applyForLong(p)
    override def applyForFloat(p: Point): Float = - x.applyForLong(p)
    override def applyForLong(p: Point): Long = - x.applyForLong(p)
  }
}
