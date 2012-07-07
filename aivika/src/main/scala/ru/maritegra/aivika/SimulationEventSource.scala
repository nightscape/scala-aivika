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

class SimulationEventSource extends EventSource[Point] {

  private var map = Map[Int, EventSource[Point]]()

  publish addListener ((sender: Any, p: Point) => {

    val runIndex = p.run.index

    if (map.contains(runIndex)) {
      map(runIndex).trigger(sender, p)
    }
  })

  def publishInRun(runIndex: Int): Event[Point] = {

    if (map.contains(runIndex)) {
      map(runIndex).publish

    } else {

      synchronized {

        if (map.contains(runIndex)) {
          map(runIndex).publish

        } else {

          val source = new EventSource[Point]

          map += runIndex -> source

          source.publish
        }
      }
    }
  }
}