/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment

import ru.maritegra.aivika._
import ru.maritegra.aivika.experiment.generator._

class Experiment {

  var verbose = true

  var title = "Simulation Experiment"

  var toc = "Contents"

  var description = ""

  var dir: ExperimentDirectory = UniqueDirectory("experiment")

  var runCount = 1

  var simulation = new Simulation

  var specs = new Specs(0, 1, 0.2, RungeKutta4)

  val items: ItemBuffer = new ItemBuffer

  def run() {

    val g = new ExperimentGenerator(this)
    g.run()
  }

  def runParallel() {

    val g = new ExperimentGenerator(this)
    g.runParallel()
  }
}