/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment

import scala.collection.mutable.Buffer

class SeriesBuffer[A] {

  protected[experiment] val buffer = Buffer[Series[A]]()

  def +=(series: Series[A]) {
    buffer += series
  }
}