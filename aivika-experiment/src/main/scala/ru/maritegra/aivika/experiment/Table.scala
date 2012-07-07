/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment

import ru.maritegra.aivika._

class Table {

  var title = "Table"

  var description = ""

  var link = "Download the CSV file"

  var runLink = "$LINK / Run $RUN_INDEX of $RUN_COUNT"

  var file: ItemFile = UniqueFile("$TITLE - $RUN_INDEX", ".csv")

  var separator = ","

  var format: PartialFunction[Any, String] = { case x => x.toString }

  var filter: Dynamics[Boolean] = Dynamics(true)

  var series = new SeriesBuffer[Any]
}