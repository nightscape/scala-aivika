/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class Byte2LongArithmetic
  extends BinaryArithmetic[Byte, Long, Long] {

  def add(x: Dynamics[Byte], y: Dynamics[Long]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.applyForByte(p) + y.applyForLong(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForByte(p) + y.applyForLong(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForByte(p) + y.applyForLong(p)

    override def applyForLong(p: Point): Long =
      x.applyForByte(p) + y.applyForLong(p)
  }

  def sub(x: Dynamics[Byte], y: Dynamics[Long]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.applyForByte(p) - y.applyForLong(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForByte(p) - y.applyForLong(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForByte(p) - y.applyForLong(p)

    override def applyForLong(p: Point): Long =
      x.applyForByte(p) - y.applyForLong(p)
  }

  def mult(x: Dynamics[Byte], y: Dynamics[Long]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.applyForByte(p) * y.applyForLong(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForByte(p) * y.applyForLong(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForByte(p) * y.applyForLong(p)

    override def applyForLong(p: Point): Long =
      x.applyForByte(p) * y.applyForLong(p)
  }

  def div(x: Dynamics[Byte], y: Dynamics[Long]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.applyForByte(p) / y.applyForLong(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForByte(p) / y.applyForLong(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForByte(p) / y.applyForLong(p)

    override def applyForLong(p: Point): Long =
      x.applyForByte(p) / y.applyForLong(p)
  }

  def rem(x: Dynamics[Byte], y: Dynamics[Long]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.applyForByte(p) % y.applyForLong(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForByte(p) % y.applyForLong(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForByte(p) % y.applyForLong(p)

    override def applyForLong(p: Point): Long =
      x.applyForByte(p) % y.applyForLong(p)
  }
}
