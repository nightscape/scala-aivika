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
      math.IEEEremainder(x.apply(p), y.apply(p))

  }

  def abs[A](x: Dynamics[A])(implicit math: NumMath[A]): Dynamics[A] =
    math.abs(x)

  def acos(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.acos(x.apply(p))

  }

  def asin(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.asin(x.apply(p))

  }

  def atan(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.atan(x.apply(p))

  }

  def atan2(y: Dynamics[Double], x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.atan2(y.apply(p), x.apply(p))

  }

  def cbrt(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.cbrt(x.apply(p))

  }

  def ceil(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.ceil(x.apply(p))

  }

  def cos(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.cos(x.apply(p))

  }

  def cosh(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.cosh(x.apply(p))

  }

  def exp(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.exp(x.apply(p))

  }

  def expm1(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.expm1(x.apply(p))

  }

  def floor(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.floor(x.apply(p))

  }

  def hypot(x: Dynamics[Double], y: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.hypot(x.apply(p), y.apply(p))

  }

  def log(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.log(x.apply(p))

  }

  def log10(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.log10(x.apply(p))

  }

  def log1p(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.log1p(x.apply(p))

  }

  def max[A](x: Dynamics[A], y: Dynamics[A])(implicit math: NumMath[A]): Dynamics[A] = math.max(x, y)

  def min[A](x: Dynamics[A], y: Dynamics[A])(implicit math: NumMath[A]): Dynamics[A] = math.min(x, y)

  def pow(x: Dynamics[Double], y: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.pow(x.apply(p), y.apply(p))

  }

  def rint(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.rint(x.apply(p))

  }

  def round[A](x: Dynamics[A])(implicit math: RealMath[A]): Dynamics[A] =
    math.round(x)

  def signum[A](x: Dynamics[A])(implicit math: NumMath[A]): Dynamics[A] =
    math.signum(x)

  def sin(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.sin(x.apply(p))

  }

  def sinh(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.sinh(x.apply(p))

  }

  def sqrt(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.sqrt(x.apply(p))

  }

  def tan(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.tan(x.apply(p))

  }

  def tanh(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.tanh(x.apply(p))

  }

  def toDegrees(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.toDegrees(x.apply(p))

  }

  def toRadians(x: Dynamics[Double]) = new Dynamics[Double] {

    def apply(p: Point): Double =
      math.toRadians(x.apply(p))

  }

  def ulp[A](x: Dynamics[A])(implicit math: RealMath[A]): Dynamics[A] =
    math.ulp(x)
}
