/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.statistics

trait TimeStatistics {

  def min: Double
  def max: Double

  def timeOfMin: Double
  def timeOfMax: Double

  def mean: Double
  def variance: Double
}
