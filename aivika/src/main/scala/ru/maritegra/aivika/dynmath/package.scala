/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

package object dynmath {

  def IEEEremainder(x: Dynamics[Double], y: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.IEEEremainder(x.applyForDouble(p), y.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.IEEEremainder(x.applyForDouble(p), y.applyForDouble(p))
  }

  def abs[A](x: Dynamics[A])(implicit math: NumMath[A]): Dynamics[A] = 
    math.abs(x)

  def acos(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.acos(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.acos(x.applyForDouble(p))
  }

  def asin(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.asin(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.asin(x.applyForDouble(p))
  }

  def atan(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.atan(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.atan(x.applyForDouble(p))
  }

  def atan2(y: Dynamics[Double], x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.atan2(y.applyForDouble(p), x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.atan2(y.applyForDouble(p), x.applyForDouble(p))
  }

  def cbrt(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.cbrt(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.cbrt(x.applyForDouble(p))
  }

  def ceil(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.ceil(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.ceil(x.applyForDouble(p))
  }

  def cos(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.cos(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.cos(x.applyForDouble(p))
  }

  def cosh(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.cosh(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.cosh(x.applyForDouble(p))
  }

  def exp(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.exp(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.exp(x.applyForDouble(p))
  }

  def expm1(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.expm1(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.expm1(x.applyForDouble(p))
  }

  def floor(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.floor(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.floor(x.applyForDouble(p))
  }

  def hypot(x: Dynamics[Double], y: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.hypot(x.applyForDouble(p), y.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.hypot(x.applyForDouble(p), y.applyForDouble(p))
  }

  def log(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.log(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.log(x.applyForDouble(p))
  }

  def log10(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.log10(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.log10(x.applyForDouble(p))
  }

  def log1p(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.log1p(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.log1p(x.applyForDouble(p))
  }

  def max[A](x: Dynamics[A], y: Dynamics[A])
    (implicit math: NumMath[A]): Dynamics[A] = math.max(x, y)

  def min[A](x: Dynamics[A], y: Dynamics[A])
    (implicit math: NumMath[A]): Dynamics[A] = math.min(x, y)

  def pow(x: Dynamics[Double], y: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.pow(x.applyForDouble(p), y.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.pow(x.applyForDouble(p), y.applyForDouble(p))
  }

  def rint(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.rint(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.rint(x.applyForDouble(p))
  }

  def round[A](x: Dynamics[A])(implicit math: RealMath[A]): Dynamics[A] =
    math.round(x)

  def signum[A](x: Dynamics[A])(implicit math: NumMath[A]): Dynamics[A] = 
    math.signum(x)

  def sin(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.sin(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.sin(x.applyForDouble(p))
  }

  def sinh(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.sinh(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.sinh(x.applyForDouble(p))
  }

  def sqrt(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.sqrt(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.sqrt(x.applyForDouble(p))
  }

  def tan(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.tan(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.tan(x.applyForDouble(p))
  }

  def tanh(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.tanh(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.tanh(x.applyForDouble(p))
  }

  def toDegrees(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.toDegrees(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.toDegrees(x.applyForDouble(p))
  }

  def toRadians(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double = 
      math.toRadians(x.applyForDouble(p))

    override def applyForDouble(p: Point): Double = 
      math.toRadians(x.applyForDouble(p))
  }

  def ulp[A](x: Dynamics[A])(implicit math: RealMath[A]): Dynamics[A] =
    math.ulp(x)
}
