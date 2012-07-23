/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

import scala.collection.immutable.Map

import ru.maritegra.aivika.util._

class RunEventSource extends EventSource[Run] {

  private var map = Map[Int, EventSource[Run]]()

  publish addListener ((sender: Any, run: Run) => {

    val runIndex = run.index

    if (map.contains(runIndex)) {
      map(runIndex).trigger(sender, run)
    }
  })

  def publishInRun(runIndex: Int): Event[Run] = {

    if (map.contains(runIndex)) {
      map(runIndex).publish

    } else {

      synchronized {

        if (map.contains(runIndex)) {
          map(runIndex).publish

        } else {

          val source = new EventSource[Run]

          map += runIndex -> source

          source.publish
        }
      }
    }
  }
}