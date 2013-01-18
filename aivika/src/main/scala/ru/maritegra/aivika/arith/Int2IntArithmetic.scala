/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class Int2IntArithmetic
  extends BinaryArithmetic[Int, Int, Int] {

  def add(x: Dynamics[Int], y: Dynamics[Int]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.apply(p) + y.apply(p)
    
    
    
    
      }

  def sub(x: Dynamics[Int], y: Dynamics[Int]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.apply(p) - y.apply(p)
    
    
    
    
      }

  def mult(x: Dynamics[Int], y: Dynamics[Int]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.apply(p) * y.apply(p)
    
    
    
    
      }

  def div(x: Dynamics[Int], y: Dynamics[Int]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.apply(p) / y.apply(p)
    
    
    
    
      }

  def rem(x: Dynamics[Int], y: Dynamics[Int]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.apply(p) % y.apply(p)
    
    
    
    
      }
}
