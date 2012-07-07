/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra

import scala.util.continuations._

package object aivika {

  /**
   * An annotation indicating to the discontinuous process.
   */
  type process = cps[Dynamics[Unit]]

  /**
   * Invokes the computation if the condition is `true`.
   */
  def when(cond: Boolean)(m: => Unit @process): Unit @process = {
    if (cond) m else {}
  }

  /**
   * Invokes the computation if the condition is `false`.
   */
  def unless(cond: Boolean)(m: => Unit @process): Unit @process = {
    if (! cond) m else {}
  }

  implicit def double2dynamics(a: Double): Dynamics[Double] = Dynamics(a)
  implicit def float2dynamics(a: Float): Dynamics[Float] = Dynamics(a)
  implicit def long2dynamics(a: Long): Dynamics[Long] = Dynamics(a)
  implicit def int2dynamics(a: Int): Dynamics[Int] = Dynamics(a)
  implicit def short2dynamics(a: Short): Dynamics[Short] = Dynamics(a)
  implicit def byte2dynamics(a: Byte): Dynamics[Byte] = Dynamics(a)

  implicit object DoubleArithmetic extends arith.DoubleArithmetic
  implicit object Double2DoubleArithmetic extends arith.Double2DoubleArithmetic
  implicit object Double2FloatArithmetic extends arith.Double2FloatArithmetic
  implicit object Double2LongArithmetic extends arith.Double2LongArithmetic
  implicit object Double2IntArithmetic extends arith.Double2IntArithmetic
  implicit object Double2ShortArithmetic extends arith.Double2ShortArithmetic
  implicit object Double2ByteArithmetic extends arith.Double2ByteArithmetic

  implicit object FloatArithmetic extends arith.FloatArithmetic
  implicit object Float2DoubleArithmetic extends arith.Float2DoubleArithmetic
  implicit object Float2FloatArithmetic extends arith.Float2FloatArithmetic
  implicit object Float2LongArithmetic extends arith.Float2LongArithmetic
  implicit object Float2IntArithmetic extends arith.Float2IntArithmetic
  implicit object Float2ShortArithmetic extends arith.Float2ShortArithmetic
  implicit object Float2ByteArithmetic extends arith.Float2ByteArithmetic

  implicit object LongArithmetic extends arith.LongArithmetic
  implicit object Long2DoubleArithmetic extends arith.Long2DoubleArithmetic
  implicit object Long2FloatArithmetic extends arith.Long2FloatArithmetic
  implicit object Long2LongArithmetic extends arith.Long2LongArithmetic
  implicit object Long2IntArithmetic extends arith.Long2IntArithmetic
  implicit object Long2ShortArithmetic extends arith.Long2ShortArithmetic
  implicit object Long2ByteArithmetic extends arith.Long2ByteArithmetic

  implicit object IntArithmetic extends arith.IntArithmetic
  implicit object Int2DoubleArithmetic extends arith.Int2DoubleArithmetic
  implicit object Int2FloatArithmetic extends arith.Int2FloatArithmetic
  implicit object Int2LongArithmetic extends arith.Int2LongArithmetic
  implicit object Int2IntArithmetic extends arith.Int2IntArithmetic
  implicit object Int2ShortArithmetic extends arith.Int2ShortArithmetic
  implicit object Int2ByteArithmetic extends arith.Int2ByteArithmetic

  implicit object ShortArithmetic extends arith.ShortArithmetic
  implicit object Short2DoubleArithmetic extends arith.Short2DoubleArithmetic
  implicit object Short2FloatArithmetic extends arith.Short2FloatArithmetic
  implicit object Short2LongArithmetic extends arith.Short2LongArithmetic
  implicit object Short2IntArithmetic extends arith.Short2IntArithmetic
  implicit object Short2ShortArithmetic extends arith.Short2ShortArithmetic
  implicit object Short2ByteArithmetic extends arith.Short2ByteArithmetic

  implicit object ByteArithmetic extends arith.ByteArithmetic
  implicit object Byte2DoubleArithmetic extends arith.Byte2DoubleArithmetic
  implicit object Byte2FloatArithmetic extends arith.Byte2FloatArithmetic
  implicit object Byte2LongArithmetic extends arith.Byte2LongArithmetic
  implicit object Byte2IntArithmetic extends arith.Byte2IntArithmetic
  implicit object Byte2ShortArithmetic extends arith.Byte2ShortArithmetic
  implicit object Byte2ByteArithmetic extends arith.Byte2ByteArithmetic

  implicit def float2double(x: Dynamics[Float]) = new Dynamics[Double] {

    def apply(p: Point): Double = x.applyForFloat(p)

    override def applyForDouble(p: Point): Double = x.applyForFloat(p)
  }

  implicit def long2double(x: Dynamics[Long]) = new Dynamics[Double] {

    def apply(p: Point): Double = x.applyForLong(p)

    override def applyForDouble(p: Point): Double = x.applyForLong(p)
  }

  implicit def int2double(x: Dynamics[Int]) = new Dynamics[Double] {

    def apply(p: Point): Double = x.applyForInt(p)

    override def applyForDouble(p: Point): Double = x.applyForInt(p)
  }

  implicit def short2double(x: Dynamics[Short]) = new Dynamics[Double] {

    def apply(p: Point): Double = x.applyForShort(p)

    override def applyForDouble(p: Point): Double = x.applyForShort(p)
  }

  implicit def byte2double(x: Dynamics[Byte]) = new Dynamics[Double] {

    def apply(p: Point): Double = x.applyForByte(p)

    override def applyForDouble(p: Point): Double = x.applyForByte(p)
  }

  implicit def long2float(x: Dynamics[Long]) = new Dynamics[Float] {

    def apply(p: Point): Float = x.applyForLong(p)

    override def applyForDouble(p: Point): Double = x.applyForLong(p)
    override def applyForFloat(p: Point): Float = x.applyForLong(p)
  }

  implicit def int2float(x: Dynamics[Int]) = new Dynamics[Float] {

    def apply(p: Point): Float = x.applyForInt(p)

    override def applyForDouble(p: Point): Double = x.applyForInt(p)
    override def applyForFloat(p: Point): Float = x.applyForInt(p)
  }

  implicit def short2float(x: Dynamics[Short]) = new Dynamics[Float] {

    def apply(p: Point): Float = x.applyForShort(p)

    override def applyForDouble(p: Point): Double = x.applyForShort(p)
    override def applyForFloat(p: Point): Float = x.applyForShort(p)
  }

  implicit def byte2float(x: Dynamics[Byte]) = new Dynamics[Float] {

    def apply(p: Point): Float = x.applyForByte(p)

    override def applyForDouble(p: Point): Double = x.applyForByte(p)
    override def applyForFloat(p: Point): Float = x.applyForByte(p)
  }

  implicit def int2long(x: Dynamics[Int]) = new Dynamics[Long] {

    def apply(p: Point): Long = x.applyForInt(p)

    override def applyForDouble(p: Point): Double = x.applyForInt(p)
    override def applyForFloat(p: Point): Float = x.applyForInt(p)
    override def applyForLong(p: Point): Long = x.applyForInt(p)
  }

  implicit def short2long(x: Dynamics[Short]) = new Dynamics[Long] {

    def apply(p: Point): Long = x.applyForShort(p)

    override def applyForDouble(p: Point): Double = x.applyForShort(p)
    override def applyForFloat(p: Point): Float = x.applyForShort(p)
    override def applyForLong(p: Point): Long = x.applyForShort(p)
  }

  implicit def byte2long(x: Dynamics[Byte]) = new Dynamics[Long] {

    def apply(p: Point): Long = x.applyForByte(p)

    override def applyForDouble(p: Point): Double = x.applyForByte(p)
    override def applyForFloat(p: Point): Float = x.applyForByte(p)
    override def applyForLong(p: Point): Long = x.applyForByte(p)
  }

  implicit def short2int(x: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = x.applyForShort(p)

    override def applyForDouble(p: Point): Double = x.applyForShort(p)
    override def applyForFloat(p: Point): Float = x.applyForShort(p)
    override def applyForLong(p: Point): Long = x.applyForShort(p)
    override def applyForInt(p: Point): Int = x.applyForShort(p)
  }

  implicit def byte2int(x: Dynamics[Byte]) = new Dynamics[Int] {

    def apply(p: Point): Int = x.applyForByte(p)

    override def applyForDouble(p: Point): Double = x.applyForByte(p)
    override def applyForFloat(p: Point): Float = x.applyForByte(p)
    override def applyForLong(p: Point): Long = x.applyForByte(p)
    override def applyForInt(p: Point): Int = x.applyForByte(p)
  }

  implicit def byte2short(x: Dynamics[Byte]) = new Dynamics[Short] {

    def apply(p: Point): Short = x.applyForByte(p)

    override def applyForDouble(p: Point): Double = x.applyForByte(p)
    override def applyForFloat(p: Point): Float = x.applyForByte(p)
    override def applyForLong(p: Point): Long = x.applyForByte(p)
    override def applyForInt(p: Point): Int = x.applyForByte(p)
    override def applyForShort(p: Point): Short = x.applyForByte(p)
  }

  implicit object DoubleMath extends dynmath.DoubleMath
  implicit object FloatMath extends dynmath.FloatMath
  implicit object LongMath extends dynmath.LongMath
  implicit object IntMath extends dynmath.IntMath
}
