/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

import ru.maritegra.aivika.util._

class Run(val specs: Specs, val index: Int, val count: Int, val simulation: Option[Simulation]) {

  def this(specs: Specs) { this(specs, 0, 1, None) }

  private val _disposingSource = new EventSource[Unit]
  private val _disposedSource = new EventSource[Unit]

  private var _disposed = false

  def dispose() {

    if (! _disposed) {

      _disposingSource.trigger(this, ())
      _disposed = true
      _disposedSource.trigger(this, ())
    }
  }

  def disposing: Event[Unit] = _disposingSource.publish
  def disposed: Event[Unit] = _disposedSource.publish

  def isDisposed(): Boolean = _disposed
}

object Run {

  val index = new Dynamics[Int] {

    def apply(p: Point): Int = p.run.index

    override def applyForDouble(p: Point): Double = p.run.index
    override def applyForFloat(p: Point): Float = p.run.index
    override def applyForLong(p: Point): Long = p.run.index
    override def applyForInt(p: Point): Int = p.run.index
  }

  val count = new Dynamics[Int] {

    def apply(p: Point): Int = p.run.count

    override def applyForDouble(p: Point): Double = p.run.count
    override def applyForFloat(p: Point): Float = p.run.count
    override def applyForLong(p: Point): Long = p.run.count
    override def applyForInt(p: Point): Int = p.run.count
  }
}
