/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.statistics

class RegularTimeStatistics extends UpdatableTimeStatistics {

  private var index = 0

  private var minX = Double.MaxValue
  private var maxX = Double.MinValue
  
  private var minXTime = Double.NaN
  private var maxXTime = Double.NaN

  private var meanX = Double.NaN
  private var meanX2 = Double.NaN

  // DX = E(X - EX)^2 = E(X^2 - 2*E*EX + (EX)^2) = E(X^2) - (EX)^2

  // S_n = x_n + S_{n-1}
  // e_n = S_n / n
  // e_{n-1} = S_{n-1} / (n - 1)
  //
  // e_n = S_n / n = (x_n + S_{n-1}) / n
  //     = x_n / n + S_{n-1}/(n-1) * (n-1)/n =
  //     = x_n / n + e_{n-1} * (n-1) / n

  def add(t: Double, x: Double) {

    if (! x.isNaN) {

      if (index == 0) {

        minX = x
        maxX = x

        minXTime = t
        maxXTime = t

        meanX = x
        meanX2 = x * x

        index += 1

      } else {

        index += 1

        val n  = index.toDouble
        val a1 = 1.0 / n
        val a2 = (n - 1.0) / n

        if (x < minX) {

          minX = x
          minXTime = t
        }

        if (x > maxX) {

          maxX = x
          maxXTime = t
        }

        meanX = a1 * x + a2 * meanX
        meanX2 = a1 * x * x + a2 * meanX2
      }
    }
  }

  def min: Double = minX
  def max: Double = maxX
  
  def timeOfMin: Double = minXTime
  def timeOfMax: Double = maxXTime
  
  def mean: Double = meanX

  def variance: Double = {

    val n = index.toDouble

    if (index > 1) {
      (meanX2 - meanX * meanX) * (n / (n - 1))

    } else {
      (meanX2 - meanX * meanX)
    }
  }
}
