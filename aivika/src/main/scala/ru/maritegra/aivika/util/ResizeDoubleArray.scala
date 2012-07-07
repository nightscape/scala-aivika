/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.util

import scala.annotation.tailrec

final class ResizeDoubleArray extends Seq[Double] {

  private var xs: Array[Double] = new Array(11)
  private var count = 0

  private def increase(capacity: Int) {

    val n = xs.length

    assert(capacity >= 0)
    val capacity2 = if (n < 64) { (n + 1) << 1 } else { 3 * (n >> 1) }

    assert(capacity2 >= 0)
    val capacity3 = if (capacity2 < capacity) capacity else capacity2

    val xs2 = new Array[Double](capacity3)

    for (i <- 0 to xs.length - 1) {
      xs2(i) = xs(i)
    }

    xs = xs2
  }

  def length: Int = count

  def apply(idx: Int): Double = xs(idx)

  def update(idx: Int, x: Double): Unit = { xs(idx) = x }

  def iterator: Iterator[Double] = xs.iterator

  def +=(x: Double): this.type = {

    if (count == xs.length) {
      increase(count + 1)
    }

    xs(count) = x
    count += 1

    this
  }

  def toArray: Array[Double] = {

    val xs2 = new Array[Double](count)

    for (i <- 0 to xs.length - 1) {
      xs2(i) = xs(i)
    }

    xs2
  }

  def binarySearch(x: Double, rightmost: Boolean): Int = binarySearch(x, 0, count - 1, rightmost)

  @tailrec private def binarySearch(x: Double, left: Int, right: Int, rightmost: Boolean): Int = {

    if (left > right) {
      ~(right + 1)

    } else {

      val index = (left + right + 1) / 2
      val curr  = xs(index)

      if (x < curr) {
        binarySearch(x, left, index - 1, rightmost)

      } else if (x > curr) {
        binarySearch(x, index + 1, right, rightmost)

      } else if (! rightmost) {
        index

      } else if (index < right) {
        binarySearch(x, index, right, rightmost)

      } else {
        index
      }
    }
  }
}
