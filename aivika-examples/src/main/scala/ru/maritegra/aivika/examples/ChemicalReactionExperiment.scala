/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.examples

import ru.maritegra.aivika._
import ru.maritegra.aivika.SystemDynamics._
import ru.maritegra.aivika.experiment._

object ChemicalReactionExperiment {

  import ChemicalReactionModel._

  val experiment = new Experiment {

    specs = Specs(0, 10, 0.001, RungeKutta4)

    title = "Chemical Reaction"

    items += new LastValues {

      title = "Last values"

      series += Series("a", a)
      series += Series("b", b)
      series += Series("c", c)
    }

    items += new Table {

      title = "Table"

      series += Series("t", time)
      series += Series("a", a)
      series += Series("b", b)
      series += Series("c", c)

      separator = ";"

      format = { case x: Double => x.toString().replace('.', ',') }
    }

    items += new TimeSeries {

      title = "Time Series"

      series += Series("a", a)
      series += Series("b", b)
      series += Series("c", c)
    }
  }

  def main(args: Array[String]) {
    experiment.run()
  }
}
