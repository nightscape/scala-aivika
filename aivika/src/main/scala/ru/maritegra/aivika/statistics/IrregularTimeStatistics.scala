/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.statistics

class IrregularTimeStatistics extends UpdatableTimeStatistics {

  private class Summator {

    private var index = 0

    private var t0 = Double.MaxValue
    private var t1 = Double.MinValue

    private var x0 = 0.0
    private var x1 = 0.0

    private var sumX = Double.NaN

    def add(t: Double, x: Double) {

      if (index == 0) {

        t0 = t
        t1 = t

        x0 = x
        x1 = x

        sumX = 0

        index += 1

      } else {

        assert(t >= t1)

        index += 1

        sumX += (t - t1) * x1

        t1 = t
        x1 = x
      }
    }

    def meanX: Double = {

      if (t1 > t0) {
        sumX / (t1 - t0)

      } else {
        x0
      }
    }
  }

  private var index = 0

  private var minX = Double.MaxValue
  private var maxX = Double.MinValue

  private var minXTime = Double.NaN
  private var maxXTime = Double.NaN

  private var sumX  = new Summator
  private var sumX2 = new Summator

  // DX = E(X - EX)^2 = E(X^2 - 2*E*EX + (EX)^2) = E(X^2) - (EX)^2

  def add(t: Double, x: Double) {

    if (! x.isNaN) {

      if (index == 0) {

        minX = x
        maxX = x

        minXTime = t
        maxXTime = t

        sumX.add(t, x)
        sumX2.add(t, x * x)

        index += 1

      } else {

        index += 1

        if (x < minX) {

          minX = x
          minXTime = t
        }

        if (x > maxX) {

          maxX = x
          maxXTime = t
        }

        sumX.add(t, x)
        sumX2.add(t, x * x)
      }
    }
  }

  def min: Double = minX
  def max: Double = maxX

  def timeOfMin: Double = minXTime
  def timeOfMax: Double = maxXTime

  def mean: Double = sumX.meanX

  def variance: Double = {

    val ex  = sumX.meanX
    val ex2 = sumX2.meanX

    (ex2 - ex * ex)
  }
}
