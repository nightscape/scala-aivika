/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class Int2FloatArithmetic
  extends BinaryArithmetic[Int, Float, Float] {
    
  def add(x: Dynamics[Int], y: Dynamics[Float]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.applyForInt(p) + y.applyForFloat(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForInt(p) + y.applyForFloat(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForInt(p) + y.applyForFloat(p)
  }
    
  def sub(x: Dynamics[Int], y: Dynamics[Float]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.applyForInt(p) - y.applyForFloat(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForInt(p) - y.applyForFloat(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForInt(p) - y.applyForFloat(p)
  }
    
  def mult(x: Dynamics[Int], y: Dynamics[Float]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.applyForInt(p) * y.applyForFloat(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForInt(p) * y.applyForFloat(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForInt(p) * y.applyForFloat(p)
  }
    
  def div(x: Dynamics[Int], y: Dynamics[Float]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.applyForInt(p) / y.applyForFloat(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForInt(p) / y.applyForFloat(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForInt(p) / y.applyForFloat(p)
  }
    
  def rem(x: Dynamics[Int], y: Dynamics[Float]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.applyForInt(p) % y.applyForFloat(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForInt(p) % y.applyForFloat(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForInt(p) % y.applyForFloat(p)
  }
}
