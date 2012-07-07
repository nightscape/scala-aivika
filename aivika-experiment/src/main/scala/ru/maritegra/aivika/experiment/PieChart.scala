/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment

class PieChart {

  var title = "Pie Chart"

  var runTitle = "$TITLE / Run $RUN_INDEX of $RUN_COUNT"

  var description = ""

  var file: ItemFile = UniqueFile("$TITLE - $RUN_INDEX", ".png")

  var width = 800

  var height = 500

  var legend = true

  var tooltips = true

  val series = new SeriesBuffer[Double]
}