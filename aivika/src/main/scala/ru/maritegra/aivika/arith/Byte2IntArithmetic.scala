/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class Byte2IntArithmetic
  extends BinaryArithmetic[Byte, Int, Int] {

  def add(x: Dynamics[Byte], y: Dynamics[Int]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.apply(p) + y.apply(p)
    
    
    
    
      }

  def sub(x: Dynamics[Byte], y: Dynamics[Int]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.apply(p) - y.apply(p)
    
    
    
    
      }

  def mult(x: Dynamics[Byte], y: Dynamics[Int]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.apply(p) * y.apply(p)
    
    
    
    
      }

  def div(x: Dynamics[Byte], y: Dynamics[Int]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.apply(p) / y.apply(p)
    
    
    
    
      }

  def rem(x: Dynamics[Byte], y: Dynamics[Int]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.apply(p) % y.apply(p)
    
    
    
    
      }
}
