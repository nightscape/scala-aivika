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
      math.abs(x.applyForInt(p))

    override def applyForDouble(p: Point): Double = 
      math.abs(x.applyForInt(p))

    override def applyForFloat(p: Point): Float = 
      math.abs(x.applyForInt(p))

    override def applyForLong(p: Point): Long = 
      math.abs(x.applyForInt(p))

    override def applyForInt(p: Point): Int = 
      math.abs(x.applyForInt(p))
  }

  def max(x: Dynamics[Int], y: Dynamics[Int]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      math.max(x.applyForInt(p), y.applyForInt(p))

    override def applyForDouble(p: Point): Double = 
      math.max(x.applyForInt(p), y.applyForInt(p))

    override def applyForFloat(p: Point): Float = 
      math.max(x.applyForInt(p), y.applyForInt(p))

    override def applyForLong(p: Point): Long = 
      math.max(x.applyForInt(p), y.applyForInt(p))

    override def applyForInt(p: Point): Int = 
      math.max(x.applyForInt(p), y.applyForInt(p))
  }

  def min(x: Dynamics[Int], y: Dynamics[Int]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      math.min(x.applyForInt(p), y.applyForInt(p))

    override def applyForDouble(p: Point): Double = 
      math.min(x.applyForInt(p), y.applyForInt(p))

    override def applyForFloat(p: Point): Float = 
      math.min(x.applyForInt(p), y.applyForInt(p))

    override def applyForLong(p: Point): Long = 
      math.min(x.applyForInt(p), y.applyForInt(p))

    override def applyForInt(p: Point): Int = 
      math.min(x.applyForInt(p), y.applyForInt(p))
  }

  def signum(x: Dynamics[Int]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      math.signum(x.applyForInt(p))

    override def applyForDouble(p: Point): Double = 
      math.signum(x.applyForInt(p))

    override def applyForFloat(p: Point): Float = 
      math.signum(x.applyForInt(p))

    override def applyForLong(p: Point): Long = 
      math.signum(x.applyForInt(p))

    override def applyForInt(p: Point): Int = 
      math.signum(x.applyForInt(p))
  }
}
