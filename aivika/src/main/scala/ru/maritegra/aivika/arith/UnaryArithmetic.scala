/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

trait UnaryArithmetic[A, B] {

  def plus(x: Dynamics[A]): Dynamics[B]

  def minus(x: Dynamics[A]): Dynamics[B]
}
