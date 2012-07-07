/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.dynmath

import ru.maritegra.aivika._

abstract class DoubleMath extends RealMath[Double] {

  def abs(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.abs(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.abs(x.applyForDouble(p))
  }

  def max(x: Dynamics[Double], y: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.max(x.applyForDouble(p), y.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.max(x.applyForDouble(p), y.applyForDouble(p))
  }

  def min(x: Dynamics[Double], y: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.min(x.applyForDouble(p), y.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.min(x.applyForDouble(p), y.applyForDouble(p))
  }

  def signum(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.signum(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.signum(x.applyForDouble(p))
  }

  def round(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.round(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.round(x.applyForDouble(p))
  }

  def ulp(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.ulp(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.ulp(x.applyForDouble(p))
  }
}
