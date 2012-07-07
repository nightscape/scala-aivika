/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.random

import java.util.Random

class ExpRandom(rnd: Random) {

  def this() { this(new Random()) }
  def this(seed: Long) { this(new Random(seed)) }

  def next(lambda: Double): Double = - math.log(rnd.nextDouble()) / lambda
}