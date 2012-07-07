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
      math.abs(x.applyForFloat(p))

    override def applyForDouble(p: Point): Double = 
      math.abs(x.applyForFloat(p))

    override def applyForFloat(p: Point): Float = 
      math.abs(x.applyForFloat(p))
  }

  def max(x: Dynamics[Float], y: Dynamics[Float]) = new Dynamics[Float] {

    def apply(p: Point): Float = 
      math.max(x.applyForFloat(p), y.applyForFloat(p))

    override def applyForDouble(p: Point): Double = 
      math.max(x.applyForFloat(p), y.applyForFloat(p))

    override def applyForFloat(p: Point): Float = 
      math.max(x.applyForFloat(p), y.applyForFloat(p))
  }

  def min(x: Dynamics[Float], y: Dynamics[Float]) = new Dynamics[Float] {

    def apply(p: Point): Float = 
      math.min(x.applyForFloat(p), y.applyForFloat(p))

    override def applyForDouble(p: Point): Double = 
      math.min(x.applyForFloat(p), y.applyForFloat(p))

    override def applyForFloat(p: Point): Float = 
      math.min(x.applyForFloat(p), y.applyForFloat(p))
  }

  def signum(x: Dynamics[Float]) = new Dynamics[Float] {

    def apply(p: Point): Float = 
      math.signum(x.applyForFloat(p))

    override def applyForDouble(p: Point): Double = 
      math.signum(x.applyForFloat(p))

    override def applyForFloat(p: Point): Float = 
      math.signum(x.applyForFloat(p))
  }

  def round(x: Dynamics[Float]) = new Dynamics[Float] {

    def apply(p: Point): Float = 
      math.round(x.applyForFloat(p))

    override def applyForDouble(p: Point): Double = 
      math.round(x.applyForFloat(p))

    override def applyForFloat(p: Point): Float = 
      math.round(x.applyForFloat(p))
  }

  def ulp(x: Dynamics[Float]) = new Dynamics[Float] {

    def apply(p: Point): Float = 
      math.ulp(x.applyForFloat(p))

    override def applyForDouble(p: Point): Double = 
      math.ulp(x.applyForFloat(p))

    override def applyForFloat(p: Point): Float = 
      math.ulp(x.applyForFloat(p))
  }
}
