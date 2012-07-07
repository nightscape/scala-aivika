/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

import ru.maritegra.aivika.util._

/**
 * Defines a stateful object.
 */
trait Stateful {

  protected[aivika] val changedSource = new SimulationEventSource

  /**
   * Handles each change of the state.
   */
  def changed: Event[Point] = changedSource.publish

  /**
   * Handles each change of the state by the specified simulation run index.
   */
  def changedInRun(runIndex: Int): Event[Point] = changedSource.publishInRun(runIndex)
}