/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class Short2ShortArithmetic
  extends BinaryArithmetic[Short, Short, Int] {

  def add(x: Dynamics[Short], y: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.applyForShort(p) + y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForShort(p) + y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForShort(p) + y.applyForShort(p)

    override def applyForLong(p: Point): Long = 
      x.applyForShort(p) + y.applyForShort(p)

    override def applyForInt(p: Point): Int = 
      x.applyForShort(p) + y.applyForShort(p)
  }

  def sub(x: Dynamics[Short], y: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.applyForShort(p) - y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForShort(p) - y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForShort(p) - y.applyForShort(p)

    override def applyForLong(p: Point): Long = 
      x.applyForShort(p) - y.applyForShort(p)

    override def applyForInt(p: Point): Int = 
      x.applyForShort(p) - y.applyForShort(p)
  }

  def mult(x: Dynamics[Short], y: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.applyForShort(p) * y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForShort(p) * y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForShort(p) * y.applyForShort(p)

    override def applyForLong(p: Point): Long = 
      x.applyForShort(p) * y.applyForShort(p)

    override def applyForInt(p: Point): Int = 
      x.applyForShort(p) * y.applyForShort(p)
  }

  def div(x: Dynamics[Short], y: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.applyForShort(p) / y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForShort(p) / y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForShort(p) / y.applyForShort(p)

    override def applyForLong(p: Point): Long = 
      x.applyForShort(p) / y.applyForShort(p)

    override def applyForInt(p: Point): Int = 
      x.applyForShort(p) / y.applyForShort(p)
  }

  def rem(x: Dynamics[Short], y: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.applyForShort(p) % y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForShort(p) % y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForShort(p) % y.applyForShort(p)

    override def applyForLong(p: Point): Long = 
      x.applyForShort(p) % y.applyForShort(p)

    override def applyForInt(p: Point): Int = 
      x.applyForShort(p) % y.applyForShort(p)
  }
}
