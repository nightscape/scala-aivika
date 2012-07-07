/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment

import ru.maritegra.aivika._

class CumulativeHistogram {

  var title = "Cumulative Histogram"

  var description = ""

  var file: ItemFile = UniqueFile("$TITLE", ".png")

  var width = 800

  var height = 500

  var legend = true

  var tooltips = true

  var bins = 31

  var histogramType: HistogramType = FrequencyHistogram

  var xAxis = SingleAxis("")

  var yAxis = SingleAxis("")

  var filter: Dynamics[Boolean] = Dynamics(true)

  val series = new SeriesBuffer[Double]
}