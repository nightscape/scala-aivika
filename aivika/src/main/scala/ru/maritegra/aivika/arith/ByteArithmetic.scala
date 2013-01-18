/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class ByteArithmetic extends UnaryArithmetic[Byte, Int] {

  def plus(x: Dynamics[Byte]) = new Dynamics[Int] {

    def apply(p: Point): Int = x.apply(p)

  }

  def minus(x: Dynamics[Byte]) = new Dynamics[Int] {

    def apply(p: Point): Int = - x.apply(p)

  }
}
