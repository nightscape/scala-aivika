/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.random

import java.util.Random

class UniformRandom(min: Double, max: Double, rnd: Random) {

  def this() { this(0, 1, new Random) }
  def this(seed: Long) { this(0, 1, new Random(seed)) }

  def this(min: Double, max: Double) { this(min, max, new Random) }
  def this(min: Double, max: Double, seed: Long) { this(min, max, new Random(seed)) }

  def next(): Double = min + (max - min) * rnd.nextDouble()
}