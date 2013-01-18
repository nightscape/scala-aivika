/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class Short2LongArithmetic
  extends BinaryArithmetic[Short, Long, Long] {

  def add(x: Dynamics[Short], y: Dynamics[Long]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.apply(p) + y.apply(p)
    
    
    
      }

  def sub(x: Dynamics[Short], y: Dynamics[Long]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.apply(p) - y.apply(p)
    
    
    
      }

  def mult(x: Dynamics[Short], y: Dynamics[Long]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.apply(p) * y.apply(p)
    
    
    
      }

  def div(x: Dynamics[Short], y: Dynamics[Long]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.apply(p) / y.apply(p)
    
    
    
      }

  def rem(x: Dynamics[Short], y: Dynamics[Long]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.apply(p) % y.apply(p)
    
    
    
      }
}
