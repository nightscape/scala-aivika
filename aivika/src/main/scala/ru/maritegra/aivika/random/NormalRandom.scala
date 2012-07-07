/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.random

import java.util.Random

class NormalRandom private(mu: Double, sigma: Double, rnd: Normal01Random) {

  def this() { this(0, 1, new Normal01Random(new Random)) }
  def this(seed: Long) { this(0, 1, new Normal01Random(new Random(seed))) }

  def this(mu: Double, sigma: Double) { this(mu, sigma, new Normal01Random(new Random)) }
  def this(mu: Double, sigma: Double, seed: Long) { this(mu, sigma, new Normal01Random(new Random(seed))) }

  def next(): Double = mu + sigma * rnd.next()
}