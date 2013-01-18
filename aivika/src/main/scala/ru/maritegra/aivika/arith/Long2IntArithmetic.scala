/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class Long2IntArithmetic
  extends BinaryArithmetic[Long, Int, Long] {

  def add(x: Dynamics[Long], y: Dynamics[Int]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.apply(p) + y.apply(p)
    
    
    
      }

  def sub(x: Dynamics[Long], y: Dynamics[Int]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.apply(p) - y.apply(p)
    
    
    
      }

  def mult(x: Dynamics[Long], y: Dynamics[Int]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.apply(p) * y.apply(p)
    
    
    
      }

  def div(x: Dynamics[Long], y: Dynamics[Int]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.apply(p) / y.apply(p)
    
    
    
      }

  def rem(x: Dynamics[Long], y: Dynamics[Int]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.apply(p) % y.apply(p)
    
    
    
      }
}
