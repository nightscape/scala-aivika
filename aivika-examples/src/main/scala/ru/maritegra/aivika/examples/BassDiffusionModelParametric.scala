/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.examples

import ru.maritegra.aivika._
import ru.maritegra.aivika.random._

class BassDiffusionModelParametric extends BassDiffusionModel {

  override val advertisingEffectiveness: Dynamics[Double] =  // 0.011
    new Parameter(new UniformRandom(0.010, 0.012).next _)

  override val contactRate: Dynamics[Double] =  // 100
    new Parameter(new UniformRandom(90, 110).next _)

  override val adoptionFraction: Dynamics[Double] =  // 0.015
    new Parameter(new UniformRandom(0.014, 0.016).next _)
}

object BassDiffusionModelParametric extends BassDiffusionModelParametric
