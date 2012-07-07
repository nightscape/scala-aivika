/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class Short2FloatArithmetic
  extends BinaryArithmetic[Short, Float, Float] {
    
  def add(x: Dynamics[Short], y: Dynamics[Float]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.applyForShort(p) + y.applyForFloat(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForShort(p) + y.applyForFloat(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForShort(p) + y.applyForFloat(p)
  }
    
  def sub(x: Dynamics[Short], y: Dynamics[Float]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.applyForShort(p) - y.applyForFloat(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForShort(p) - y.applyForFloat(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForShort(p) - y.applyForFloat(p)
  }
    
  def mult(x: Dynamics[Short], y: Dynamics[Float]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.applyForShort(p) * y.applyForFloat(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForShort(p) * y.applyForFloat(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForShort(p) * y.applyForFloat(p)
  }
    
  def div(x: Dynamics[Short], y: Dynamics[Float]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.applyForShort(p) / y.applyForFloat(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForShort(p) / y.applyForFloat(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForShort(p) / y.applyForFloat(p)
  }
    
  def rem(x: Dynamics[Short], y: Dynamics[Float]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.applyForShort(p) % y.applyForFloat(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForShort(p) % y.applyForFloat(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForShort(p) % y.applyForFloat(p)
  }
}
