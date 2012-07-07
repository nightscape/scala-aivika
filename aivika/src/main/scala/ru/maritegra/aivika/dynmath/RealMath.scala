/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.dynmath

import ru.maritegra.aivika._

trait RealMath[A] extends NumMath[A] {

  def round(x: Dynamics[A]): Dynamics[A]

  def ulp(x: Dynamics[A]): Dynamics[A]
}
