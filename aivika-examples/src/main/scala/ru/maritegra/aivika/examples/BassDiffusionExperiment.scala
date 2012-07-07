/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.examples

import ru.maritegra.aivika._
import ru.maritegra.aivika.experiment._

object BassDiffusionExperiment {

  import BassDiffusionModel._

  val experiment = new Experiment {

    title = "Bass Diffusion"

    specs = Specs(0, 8, 0.1, RungeKutta4)

    simulation = new Simulation {
      onInitPoint.add ((p: Point) => activate(p))
    }

    items += new TimeSeries {

      title = "Adoption"

      yAxis = SingleAxis("Number")

      series += Series("Potential adopters", potentialAdopters)
      series += Series("Adopters", adopters)
    }
  }

  def main(args: Array[String]) {
    experiment.run()
  }
}
