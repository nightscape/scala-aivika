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

class ChemicalReactionModelParametric extends ChemicalReactionModel {

  override val ka: Dynamics[Double] =
    new Parameter(new UniformRandom(0.9, 1.1).next _)

  override val kb: Dynamics[Double] =
    new Parameter(new UniformRandom(0.9, 1.1).next _)
}

object ChemicalReactionModelParametric extends ChemicalReactionModelParametric
