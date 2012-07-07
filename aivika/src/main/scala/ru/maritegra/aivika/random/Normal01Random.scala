/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.random

import java.util.Random

private class Normal01Random(rnd: Random) {

  def this() { this(new Random) }
  def this(seed: Long) { this(new Random(seed)) }
  
  private var num = 0.0
  private var flag = false

  def next(): Double = {

    if (flag) {

      flag = false
      num

    } else {

      var xi1 = 0.0
      var xi2 = 0.0
      var psi = 0.0

      while ((psi >= 1.0) || (psi < toc)) {

        xi1 = 2 * rnd.nextDouble() - 1
        xi2 = 2 * rnd.nextDouble() - 1
        psi = xi1 * xi1 + xi2 * xi2
      }
      
      psi = math.sqrt(- 2 * math.log(psi) / psi)

      flag = true
      num = xi2 * psi

      xi1 * psi
    }
  }

  private val toc = 1.0e-305
}