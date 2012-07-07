/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment

import ru.maritegra.aivika._

class Statistics {

  var title = "Statistics"

  var runTitle = "$TITLE / Run $RUN_INDEX of $RUN_COUNT"

  var description = ""

  var variableInfo = "Statistics: $NAME"

  var minimumInfo   = "Minimum   = $VALUE (at time $TIME)"

  var maximumInfo   = "Maximum   = $VALUE (at time $TIME)"

  var averageInfo   = "Average   = $VALUE"

  var deviationInfo = "Deviation = $VALUE"

  var format: PartialFunction[Double, String] = { case x => x.toString }

  var filter: Dynamics[Boolean] = Dynamics(true)

  val series = new SeriesBuffer[Double]
}