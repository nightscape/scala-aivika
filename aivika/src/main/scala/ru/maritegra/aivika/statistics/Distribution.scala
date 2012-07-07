/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.statistics

import scala.collection.mutable.Buffer

object Distribution {

  def toFunction(xs: Seq[Double]): Seq[(Double, Double)] = {

    val xxs = xs.sortBy(x => x)
    val n = xs.length

    val ps = Buffer[(Double, Double)]()
    var i = 0

    if (n > 1) {

      val p = (xxs(0) - (xxs(1) - xxs(0)), i / n.toDouble)

      ps += p
      i += 1
    }

    for (x <- xxs) {

      val p = (x, i / n.toDouble)

      ps += p
      i += 1
    }

    if (n > 1) {

      val p = (xxs(n - 1) + (xxs(n - 1) - xxs(n - 2)), 1.0)

      ps += p
      i += 1
    }

    ps
  }
}
