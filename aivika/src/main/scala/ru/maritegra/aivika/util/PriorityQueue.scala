/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.util

import java.util._

import scala.annotation.tailrec

class PriorityQueue[E <: AnyRef] {

  private var ks: Array[Double] = new Array(11)
  private var vs: Array[AnyRef] = new Array(11)

  private var len = 0

  private def increase(capacity: Int) {

    val n = ks.length

    assert(capacity >= 0)
    val capacity2 = if (n < 64) { (n + 1) << 1 } else { 3 * (n >> 1) }

    assert(capacity2 >= 0)
    val capacity3 = if (capacity2 < capacity) capacity else capacity2

    ks = Arrays.copyOf(ks, capacity3)
    vs = Arrays.copyOf(vs, capacity3)
  }

  @tailrec private def siftUp(i: Int, k: Double, v: AnyRef) {

    if (i == 0) {

      ks(i) = k
      vs(i) = v

    } else {

      val n = (i - 1) >> 1

      if (k >= ks(n)) {

        ks(i) = k
        vs(i) = v

      } else {

        ks(i) = ks(n)
        vs(i) = vs(n)

        siftUp(n, k, v)
      }
    }
  }

  @tailrec private def siftDown(i: Int, k: Double, v: AnyRef) {

    if (i >= (len >> 1)) {

      ks(i) = k
      vs(i) = v

    } else {

      val n3 = (i << 1) + 1
      val n2 = n3 + 1
      val n  = if (n2 < len && ks(n3) > ks(n2)) n2 else n3

      if (k <= ks(n)) {

        ks(i) = k
        vs(i) = v
	
      } else {

        ks(i) = ks(n)
        vs(i) = vs(n)

        siftDown(n, k, v)
      }
    }
  }

  def length: Int = len

  def isEmpty: Boolean = (len == 0)

  def enqueue(k: Double, v: E) {

    val i = len

    if (i >= ks.length) {
      increase(i + 1)
    }

    len = i + 1

    siftUp(i, k, v)
  }

  def dequeue() {

    assert(len > 0)
    len = len - 1

    val k = ks(len)
    val v = vs(len)

    ks(len) = 0
    vs(len) = null

    if (len > 0) {
      siftDown(0, k, v)
    }
  }

  def front: (Double, E) = {

    assert(len > 0)
    (ks(0), vs(0).asInstanceOf[E])
  }

  def frontKey: Double = {

    assert(len > 0)
    ks(0)
  }

  def frontValue: E = {

    assert(len > 0)
    vs(0).asInstanceOf[E]
  }
}
