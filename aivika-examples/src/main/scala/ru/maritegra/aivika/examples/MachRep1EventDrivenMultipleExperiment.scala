/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

/*
There are two machines, which sometimes break down. Up time is exponentially
distributed with mean 1.0, and repair time is exponentially distributed with
mean 0.5. There are two repairpersons, so the two machines can be repaired
simultaneously if they are down at the same time. Output is long-run
proportion of up time. Should get value of about 0.66.
 */

package ru.maritegra.aivika.examples

import ru.maritegra.aivika._
import ru.maritegra.aivika.experiment._

object MachRep1EventDrivenMultipleExperiment {

  import MachRep1EventDrivenModel._

  val experiment = new Experiment {

    runCount = 1000

    title = "Repair of the Machine (Event-driven Approach, " +
      runCount + " Simulation Runs)"

    specs = Specs(starttime = 0, stoptime = 10000,
      dt = 0.1, method = Euler)

    simulation = new Simulation {
      onInitPoint add activate
    }

    items += new CumulativeStatistics {

      series += Series("Long-run proportion of up time",
        upTimeProportion)
    }

    items += new CumulativeHistogram {

      series += Series("Long-run proportion of up time",
        upTimeProportion)
    }
  }

  def main(args: Array[String]) {
    experiment.runParallel()
  }
}
