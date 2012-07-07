/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class IntArithmetic extends UnaryArithmetic[Int, Int] {

  def plus(x: Dynamics[Int]) = x

  def minus(x: Dynamics[Int]) = new Dynamics[Int] {

    def apply(p: Point): Int = - x.applyForInt(p)

    override def applyForDouble(p: Point): Double = - x.applyForInt(p)
    override def applyForFloat(p: Point): Float = - x.applyForInt(p)
    override def applyForLong(p: Point): Long = - x.applyForInt(p)
    override def applyForInt(p: Point): Int = - x.applyForInt(p)
  }
}
