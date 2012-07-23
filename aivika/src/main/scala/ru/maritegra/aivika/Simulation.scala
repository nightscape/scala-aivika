/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

import ru.maritegra.aivika.util._

class Simulation {

  private val pausedSource = new PointEventSource

  private val initPointSource = new PointEventSource
  private val integPointSource = new PointEventSource
  private val lastPointSource = new PointEventSource

  def paused: Event[Point] = pausedSource.publish
  def pausedInRun(runIndex: Int): Event[Point] = pausedSource.publishInRun(runIndex)

  def onInitPoint: Event[Point] = initPointSource.publish
  def onIntegPoint: Event[Point] = integPointSource.publish
  def onLastPoint: Event[Point] = lastPointSource.publish

  def onInitPointInRun(runIndex: Int): Event[Point] = initPointSource.publishInRun(runIndex)
  def onIntegPointInRun(runIndex: Int): Event[Point] = integPointSource.publishInRun(runIndex)
  def onLastPointInRun(runIndex: Int): Event[Point] = lastPointSource.publishInRun(runIndex)

  private val runStartedSource = new RunEventSource
  private val runFinishedSource = new RunEventSource

  def runStarted: Event[Run] = runStartedSource.publish
  def runFinished: Event[Run] = runFinishedSource.publish

  def runStartedInRun(runIndex: Int): Event[Run] = runStartedSource.publishInRun(runIndex)
  def runFinishedInRun(runIndex: Int): Event[Run] = runFinishedSource.publishInRun(runIndex)

  private val startedSource = new EventSource[Unit]
  private val finishedSource = new EventSource[Unit]

  def started: Event[Unit] = startedSource.publish
  def finished: Event[Unit] = finishedSource.publish

  def run(specs: Specs) {

    startedSource.trigger(this, ())

    start(new Run(specs, 0, 1, Some(this)))

    finishedSource.trigger(this, ())
  }

  def run(specs: Specs, runCount: Int) {

    startedSource.trigger(this, ())

    for (runIndex <- 0 to runCount - 1) {
      start(new Run(specs, runIndex, runCount, Some(this)))
    }

    finishedSource.trigger(this, ())
  }

  def runParallel(specs: Specs, runCount: Int) {

    startedSource.trigger(this, ())

    for (runIndex <- (0 to runCount - 1).par) {
      start(new Run(specs, runIndex, runCount, Some(this)))
    }

    finishedSource.trigger(this, ())
  }

  private def start(run: Run) {

    try {

      runStartedSource.trigger(this, run)

      val specs = run.specs
      val n = specs.iterations

      if (n > 0) {

        val p = Point(specs, run, specs.time(0, 0), 0, 0)
        initPointSource.trigger(this, p)
      }

      for (i <- 0 to n-1) {

        val p = Point(specs, run, specs.time(i, 0), i, 0)
        integPointSource.trigger(this, p)
      }

      if (n > 0) {

        val p = Point(specs, run, specs.time(n-1, 0), n-1, 0)
        lastPointSource.trigger(this, p)
      }

      runFinishedSource.trigger(this, run)

    } finally {
      run.dispose()
    }
  }
}

object Simulation {

  val specs = new Dynamics[Specs] {

    def apply(p: Point): Specs = p.specs
  }
}
