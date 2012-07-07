/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class Float2ShortArithmetic
  extends BinaryArithmetic[Float, Short, Float] {
    
  def add(x: Dynamics[Float], y: Dynamics[Short]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.applyForFloat(p) + y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForFloat(p) + y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForFloat(p) + y.applyForShort(p)
  }
    
  def sub(x: Dynamics[Float], y: Dynamics[Short]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.applyForFloat(p) - y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForFloat(p) - y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForFloat(p) - y.applyForShort(p)
  }
    
  def mult(x: Dynamics[Float], y: Dynamics[Short]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.applyForFloat(p) * y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForFloat(p) * y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForFloat(p) * y.applyForShort(p)
  }
    
  def div(x: Dynamics[Float], y: Dynamics[Short]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.applyForFloat(p) / y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForFloat(p) / y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForFloat(p) / y.applyForShort(p)
  }
    
  def rem(x: Dynamics[Float], y: Dynamics[Short]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.applyForFloat(p) % y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForFloat(p) % y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForFloat(p) % y.applyForShort(p)
  }
}
