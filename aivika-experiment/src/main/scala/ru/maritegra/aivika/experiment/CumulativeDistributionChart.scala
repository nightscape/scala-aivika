/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment

import ru.maritegra.aivika._

class CumulativeDistributionChart {

  var title = "Cumulative Distribution Chart"

  var description = ""

  var file: ItemFile = UniqueFile("$TITLE", ".png")

  var width = 800

  var height = 500

  var legend = true

  var tooltips = true

  var distribution: DistributionType = ContinuousDistribution

  var xAxis = SingleAxis("")

  var yAxis = SingleAxis("Probability")

  var filter: Dynamics[Boolean] = Dynamics(true)

  val series = new SeriesBuffer[Double]
}