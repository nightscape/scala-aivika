/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class ShortArithmetic extends UnaryArithmetic[Short, Int] {

  def plus(x: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = x.applyForShort(p)

    override def applyForDouble(p: Point): Double = x.applyForShort(p)
    override def applyForFloat(p: Point): Float = x.applyForShort(p)
    override def applyForLong(p: Point): Long = x.applyForShort(p)
    override def applyForInt(p: Point): Int = x.applyForShort(p)
  }

  def minus(x: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = - x.applyForShort(p)

    override def applyForDouble(p: Point): Double = - x.applyForShort(p)
    override def applyForFloat(p: Point): Float = - x.applyForShort(p)
    override def applyForLong(p: Point): Long = - x.applyForShort(p)
    override def applyForInt(p: Point): Int = - x.applyForShort(p)
  }
}
