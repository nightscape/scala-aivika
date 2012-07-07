/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class Int2ShortArithmetic
  extends BinaryArithmetic[Int, Short, Int] {

  def add(x: Dynamics[Int], y: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.applyForInt(p) + y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForInt(p) + y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForInt(p) + y.applyForShort(p)

    override def applyForLong(p: Point): Long = 
      x.applyForInt(p) + y.applyForShort(p)

    override def applyForInt(p: Point): Int = 
      x.applyForInt(p) + y.applyForShort(p)
  }

  def sub(x: Dynamics[Int], y: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.applyForInt(p) - y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForInt(p) - y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForInt(p) - y.applyForShort(p)

    override def applyForLong(p: Point): Long = 
      x.applyForInt(p) - y.applyForShort(p)

    override def applyForInt(p: Point): Int = 
      x.applyForInt(p) - y.applyForShort(p)
  }

  def mult(x: Dynamics[Int], y: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.applyForInt(p) * y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForInt(p) * y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForInt(p) * y.applyForShort(p)

    override def applyForLong(p: Point): Long = 
      x.applyForInt(p) * y.applyForShort(p)

    override def applyForInt(p: Point): Int = 
      x.applyForInt(p) * y.applyForShort(p)
  }

  def div(x: Dynamics[Int], y: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.applyForInt(p) / y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForInt(p) / y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForInt(p) / y.applyForShort(p)

    override def applyForLong(p: Point): Long = 
      x.applyForInt(p) / y.applyForShort(p)

    override def applyForInt(p: Point): Int = 
      x.applyForInt(p) / y.applyForShort(p)
  }

  def rem(x: Dynamics[Int], y: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.applyForInt(p) % y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForInt(p) % y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForInt(p) % y.applyForShort(p)

    override def applyForLong(p: Point): Long = 
      x.applyForInt(p) % y.applyForShort(p)

    override def applyForInt(p: Point): Int = 
      x.applyForInt(p) % y.applyForShort(p)
  }
}
