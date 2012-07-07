/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class Float2DoubleArithmetic
  extends BinaryArithmetic[Float, Double, Double] {

  def add(x: Dynamics[Float], y: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      x.applyForFloat(p) + y.applyForDouble(p)

    override def applyForDouble(p: Point): Double = 
      x.applyForFloat(p) + y.applyForDouble(p)
  }

  def sub(x: Dynamics[Float], y: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      x.applyForFloat(p) - y.applyForDouble(p)

    override def applyForDouble(p: Point): Double = 
      x.applyForFloat(p) - y.applyForDouble(p)
  }

  def mult(x: Dynamics[Float], y: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      x.applyForFloat(p) * y.applyForDouble(p)

    override def applyForDouble(p: Point): Double = 
      x.applyForFloat(p) * y.applyForDouble(p)
  }

  def div(x: Dynamics[Float], y: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      x.applyForFloat(p) / y.applyForDouble(p)

    override def applyForDouble(p: Point): Double = 
      x.applyForFloat(p) / y.applyForDouble(p)
  }

  def rem(x: Dynamics[Float], y: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      x.applyForFloat(p) % y.applyForDouble(p)

    override def applyForDouble(p: Point): Double = 
      x.applyForFloat(p) % y.applyForDouble(p)
  }
}
