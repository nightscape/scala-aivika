/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

/*
Two machines, but sometimes break down. Up time is exponentially distributed
with mean 1.0, and repair time is exponentially distributed with mean 0.5.
In this example, there is only one repair-person, so the two machines cannot
be repaired simultaneously if they are down at the same time.

In addition to finding the long-run proportion of up time, let us also find
the long-run proportion of the time that a given machine has an immediate
access to the repair-person when the machine breaks down. Output values
should be about 0.6 and 0.67.
 */

package ru.maritegra.aivika.examples

import ru.maritegra.aivika._
import ru.maritegra.aivika.experiment._

object MachRep2Experiment {

  import MachRep2Model._

  val experiment = new Experiment {

    title = "Repair of Machine under Resource Constraints"

    specs = Specs(starttime = 0, stoptime = 10000,
      dt = 0.1, method = Euler)

    simulation = new Simulation {
      onInitPoint add activate
    }

    items += new LastValues {

      series += Series("Long-run proportion of up time",
        upTimeProportion)

      series += Series("Repair-person availability",
        repProportion)
    }
  }

  def main(args: Array[String]) {
    experiment.run()
  }
}
