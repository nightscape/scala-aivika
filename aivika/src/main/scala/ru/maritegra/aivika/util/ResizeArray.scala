/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.util

final class ResizeArray[A: Manifest] extends Seq[A] {

  private var xs: Array[A] = new Array(11)
  private var count = 0

  private def increase(capacity: Int) {

    val n = xs.length

    assert(capacity >= 0)
    val capacity2 = if (n < 64) { (n + 1) << 1 } else { 3 * (n >> 1) }

    assert(capacity2 >= 0)
    val capacity3 = if (capacity2 < capacity) capacity else capacity2

    val xs2 = new Array[A](capacity3)

    for (i <- 0 to xs.length - 1) {
      xs2(i) = xs(i)
    }

    xs = xs2
  }

  def length: Int = count

  def apply(idx: Int): A = xs(idx)

  def update(idx: Int, x: A): Unit = { xs(idx) = x }

  def iterator: Iterator[A] = xs.iterator

  def +=(x: A): this.type = {

    if (count == xs.length) {
      increase(count + 1)
    }

    xs(count) = x
    count += 1

    this
  }

  def toArray: Array[A] = {

    val xs2 = new Array[A](count)

    for (i <- 0 to xs.length - 1) {
      xs2(i) = xs(i)
    }

    xs2
  }
}
