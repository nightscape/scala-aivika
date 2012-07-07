/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment

class LastValues {

  var title = "Last Values"

  var runTitle = "$TITLE / Run $RUN_INDEX of $RUN_COUNT"

  var description = ""

  var format: PartialFunction[Any, String] = { case x => x.toString }

  val series = new SeriesBuffer[Any]
}