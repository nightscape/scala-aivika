/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.arith

import ru.maritegra.aivika._

abstract class ShortArithmetic extends UnaryArithmetic[Short, Int] {

  def plus(x: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = x.apply(p)

  }

  def minus(x: Dynamics[Short]) = new Dynamics[Int] {

    def apply(p: Point): Int = - x.apply(p)

  }
}
