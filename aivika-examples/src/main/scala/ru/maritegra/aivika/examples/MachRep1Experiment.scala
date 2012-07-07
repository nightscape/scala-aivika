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
import ru.maritegra.aivika.SystemDynamics._
import ru.maritegra.aivika.experiment._

object MachRep1Experiment {

  import MachRep1Model._

  val experiment = new Experiment {

    title = "Repair of the Machine (Process-driven Approach)"

    specs = Specs(starttime = 0, stoptime = 10000,
      dt = 0.1, method = Euler)

    simulation = new Simulation {
      onInitPoint add activate
    }

    items += new LastValues {

      series += Series("Long-run proportion of up time",
        upTimeProportion)
    }

    items += new Table {

      series += Series("Time", time)

      series += Series("Long-run proportion of up time",
        upTimeProportion)

//      Uncomment the following line if you want to save
//      data only in the integration time points
//
//      filter = timeIntegration
    }
  }

  def main(args: Array[String]) {
    experiment.run()
  }
}
