/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

/*
 * Vensim 5 Modeling Guide, Chapter Financial Modeling and Risk
 */

package ru.maritegra.aivika.examples

import ru.maritegra.aivika._
import ru.maritegra.aivika.experiment._

object FinancialExperiment {

  import FinancialModel._

  val experiment = new Experiment {

    specs = Specs(0, 5, 0.015625, RungeKutta4)

    title = "Financial Modeling and Risk"

    items += new TimeSeries {

      title = "Net Income and Cach Flow"

      yAxis = SingleAxis("")

      series += Series("net income", netIncome)
      series += Series("net cash flow", netCashFlow)
    }

    items += new TimeSeries {

      title = "Net Present Value of Income and Cash Flow"

      yAxis = SingleAxis("")

      series += Series("npv income", npvIncome)
      series += Series("npv cash flow", npvCashFlow)
    }
  }

  def main(args: Array[String]) {
    experiment.run()
  }
}
