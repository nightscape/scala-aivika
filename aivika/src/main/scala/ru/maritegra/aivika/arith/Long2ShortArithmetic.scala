/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class Long2ShortArithmetic
  extends BinaryArithmetic[Long, Short, Long] {

  def add(x: Dynamics[Long], y: Dynamics[Short]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.applyForLong(p) + y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForLong(p) + y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForLong(p) + y.applyForShort(p)

    override def applyForLong(p: Point): Long =
      x.applyForLong(p) + y.applyForShort(p)
  }

  def sub(x: Dynamics[Long], y: Dynamics[Short]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.applyForLong(p) - y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForLong(p) - y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForLong(p) - y.applyForShort(p)

    override def applyForLong(p: Point): Long =
      x.applyForLong(p) - y.applyForShort(p)
  }

  def mult(x: Dynamics[Long], y: Dynamics[Short]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.applyForLong(p) * y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForLong(p) * y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForLong(p) * y.applyForShort(p)

    override def applyForLong(p: Point): Long =
      x.applyForLong(p) * y.applyForShort(p)
  }

  def div(x: Dynamics[Long], y: Dynamics[Short]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.applyForLong(p) / y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForLong(p) / y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForLong(p) / y.applyForShort(p)

    override def applyForLong(p: Point): Long =
      x.applyForLong(p) / y.applyForShort(p)
  }

  def rem(x: Dynamics[Long], y: Dynamics[Short]) = new Dynamics[Long] {

    def apply(p: Point): Long = 
      x.applyForLong(p) % y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForLong(p) % y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForLong(p) % y.applyForShort(p)

    override def applyForLong(p: Point): Long =
      x.applyForLong(p) % y.applyForShort(p)
  }
}
