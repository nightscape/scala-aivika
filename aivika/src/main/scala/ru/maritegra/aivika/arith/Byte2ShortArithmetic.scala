/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class Byte2ShortArithmetic
  extends BinaryArithmetic[Byte, Short, Int] {

  def add(x: Dynamics[Byte], y: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.applyForByte(p) + y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForByte(p) + y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForByte(p) + y.applyForShort(p)

    override def applyForLong(p: Point): Long = 
      x.applyForByte(p) + y.applyForShort(p)

    override def applyForInt(p: Point): Int = 
      x.applyForByte(p) + y.applyForShort(p)
  }

  def sub(x: Dynamics[Byte], y: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.applyForByte(p) - y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForByte(p) - y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForByte(p) - y.applyForShort(p)

    override def applyForLong(p: Point): Long = 
      x.applyForByte(p) - y.applyForShort(p)

    override def applyForInt(p: Point): Int = 
      x.applyForByte(p) - y.applyForShort(p)
  }

  def mult(x: Dynamics[Byte], y: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.applyForByte(p) * y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForByte(p) * y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForByte(p) * y.applyForShort(p)

    override def applyForLong(p: Point): Long = 
      x.applyForByte(p) * y.applyForShort(p)

    override def applyForInt(p: Point): Int = 
      x.applyForByte(p) * y.applyForShort(p)
  }

  def div(x: Dynamics[Byte], y: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.applyForByte(p) / y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForByte(p) / y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForByte(p) / y.applyForShort(p)

    override def applyForLong(p: Point): Long = 
      x.applyForByte(p) / y.applyForShort(p)

    override def applyForInt(p: Point): Int = 
      x.applyForByte(p) / y.applyForShort(p)
  }

  def rem(x: Dynamics[Byte], y: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = 
      x.applyForByte(p) % y.applyForShort(p)
    
    override def applyForDouble(p: Point): Double = 
      x.applyForByte(p) % y.applyForShort(p)

    override def applyForFloat(p: Point): Float = 
      x.applyForByte(p) % y.applyForShort(p)

    override def applyForLong(p: Point): Long = 
      x.applyForByte(p) % y.applyForShort(p)

    override def applyForInt(p: Point): Int = 
      x.applyForByte(p) % y.applyForShort(p)
  }
}
