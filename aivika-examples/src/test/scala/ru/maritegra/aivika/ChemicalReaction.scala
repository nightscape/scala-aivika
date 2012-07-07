/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

import SystemDynamics._

object ChemicalReaction {

  lazy val a: Dynamics[Double] = integ(- ka * a, 100)
  lazy val b: Dynamics[Double] = integ(ka * a - kb * b, 0)
  lazy val c: Dynamics[Double] = integ(kb * b, 0)

  val ka = 1
  val kb = 1
}
