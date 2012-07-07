/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

/*
Variation of models MachRep1Model, MachRep2Model. Two machines, but
sometimes break down. Up time is exponentially distributed with mean
1.0, and repair time is exponentially distributed with mean 0.5. In
this example, there is only one repair person, and she is not summoned
until both machines are down. We find the proportion of up time. It
should come out to about 0.45.
 */

package ru.maritegra.aivika.examples

import ru.maritegra.aivika._
import ru.maritegra.aivika.experiment._

object MachRep3Experiment {

  import MachRep3Model._

  val experiment = new Experiment {

    title = "Repair of Machine, version 3"

    specs = Specs(starttime = 0, stoptime = 10000,
      dt = 0.1, method = Euler)

    simulation = new Simulation {
      onInitPoint add activate
    }

    items += new LastValues {

      series += Series("Long-run proportion of up time",
        upTimeProportion)
    }
  }

  def main(args: Array[String]) {
    experiment.run()
  }
}
