/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

trait BinaryArithmetic[A, B, C] {

  def add(x: Dynamics[A], y: Dynamics[B]): Dynamics[C]

  def sub(x: Dynamics[A], y: Dynamics[B]): Dynamics[C]

  def mult(x: Dynamics[A], y: Dynamics[B]): Dynamics[C]

  def div(x: Dynamics[A], y: Dynamics[B]): Dynamics[C]

  def rem(x: Dynamics[A], y: Dynamics[B]): Dynamics[C]
}
