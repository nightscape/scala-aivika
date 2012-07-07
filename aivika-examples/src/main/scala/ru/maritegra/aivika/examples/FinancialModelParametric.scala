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
import ru.maritegra.aivika.random._

class FinancialModelParametric extends FinancialModel {

  override val averagePayableDelay: Dynamics[Double] =
    new Parameter(new UniformRandom(0.07, 0.11).next _)

  override val billingProcessingTime: Dynamics[Double] =
    new Parameter(new UniformRandom(0.03, 0.05).next _)

  override val buildingTime: Dynamics[Double] =
    new Parameter(new UniformRandom(0.8, 1.2).next _)

  override val fractionalLossRate: Dynamics[Double] =
    new Parameter(new UniformRandom(0.05, 0.08).next _)

  override val interestRate: Dynamics[Double] =
    new Parameter(new UniformRandom(0.09, 0.15).next _)

  override val price: Dynamics[Double] =
    new Parameter(new UniformRandom(0.9, 1.2).next _)

  override val productionCapacity: Dynamics[Double] =
    new Parameter(new UniformRandom(2200, 2600).next _)

  override val requiredInvestment: Dynamics[Double] =
    new Parameter(new UniformRandom(1800, 2200).next _)

  override val variableProductionCost: Dynamics[Double] =
    new Parameter(new UniformRandom(0.5, 0.7).next _)
}

object FinancialModelParametric extends FinancialModelParametric
