/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.dynmath

import ru.maritegra.aivika._

trait NumMath[A] {

  def abs(x: Dynamics[A]): Dynamics[A]

  def max(x: Dynamics[A], y: Dynamics[A]): Dynamics[A]

  def min(x: Dynamics[A], y: Dynamics[A]): Dynamics[A]

  def signum(x: Dynamics[A]): Dynamics[A]
}
