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

class ChemicalReactionModel {

  lazy val a: Dynamics[Double] = integ(- ka * a, 100)
  lazy val b: Dynamics[Double] = integ(ka * a - kb * b, 0)
  lazy val c: Dynamics[Double] = integ(kb * b, 0)

  val ka: Dynamics[Double] = 1
  val kb: Dynamics[Double] = 1
}

object ChemicalReactionModel extends ChemicalReactionModel
