/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment

import ru.maritegra.aivika._

class CumulativeStatistics {

  var title = "Cumulative Statistics"

  var runTitle = "$TITLE"

  var description = ""

  var variableInfo = "Cumulative Statistics (the final time point): $NAME"

  var minimumInfo   = "Minimum   = $VALUE"

  var maximumInfo   = "Maximum   = $VALUE"

  var averageInfo   = "Average   = $VALUE"

  var deviationInfo = "Deviation = $VALUE"

  var format: PartialFunction[Double, String] = { case x => x.toString }

  var filter: Dynamics[Boolean] = Dynamics(true)

  val series = new SeriesBuffer[Double]
}