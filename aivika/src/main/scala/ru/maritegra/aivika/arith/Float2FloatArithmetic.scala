/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class Float2FloatArithmetic
  extends BinaryArithmetic[Float, Float, Float] {
    
  def add(x: Dynamics[Float], y: Dynamics[Float]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.apply(p) + y.apply(p)
    
    
      }
    
  def sub(x: Dynamics[Float], y: Dynamics[Float]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.apply(p) - y.apply(p)
    
    
      }
    
  def mult(x: Dynamics[Float], y: Dynamics[Float]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.apply(p) * y.apply(p)
    
    
      }
    
  def div(x: Dynamics[Float], y: Dynamics[Float]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.apply(p) / y.apply(p)
    
    
      }
    
  def rem(x: Dynamics[Float], y: Dynamics[Float]) = new Dynamics[Float] {
    
    def apply(p: Point): Float = 
      x.apply(p) % y.apply(p)
    
    
      }
}
