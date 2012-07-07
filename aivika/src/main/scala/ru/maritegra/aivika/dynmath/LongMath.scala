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
      math.abs(x.applyForLong(p))

    override def applyForDouble(p: Point): Double = 
      math.abs(x.applyForLong(p))

    override def applyForFloat(p: Point): Float = 
      math.abs(x.applyForLong(p))

    override def applyForLong(p: Point): Long = 
      math.abs(x.applyForLong(p))
  }

  def max(x: Dynamics[Long], y: Dynamics[Long]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      math.max(x.applyForLong(p), y.applyForLong(p))

    override def applyForDouble(p: Point): Double = 
      math.max(x.applyForLong(p), y.applyForLong(p))

    override def applyForFloat(p: Point): Float = 
      math.max(x.applyForLong(p), y.applyForLong(p))

    override def applyForLong(p: Point): Long = 
      math.max(x.applyForLong(p), y.applyForLong(p))
  }

  def min(x: Dynamics[Long], y: Dynamics[Long]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      math.min(x.applyForLong(p), y.applyForLong(p))

    override def applyForDouble(p: Point): Double = 
      math.min(x.applyForLong(p), y.applyForLong(p))

    override def applyForFloat(p: Point): Float = 
      math.min(x.applyForLong(p), y.applyForLong(p))

    override def applyForLong(p: Point): Long = 
      math.min(x.applyForLong(p), y.applyForLong(p))
  }

  def signum(x: Dynamics[Long]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      math.signum(x.applyForLong(p))

    override def applyForDouble(p: Point): Double = 
      math.signum(x.applyForLong(p))

    override def applyForFloat(p: Point): Float = 
      math.signum(x.applyForLong(p))

    override def applyForLong(p: Point): Long = 
      math.signum(x.applyForLong(p))
  }
}
