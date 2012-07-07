/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

/**
 * A big-P transformation that converts dynamic processes to discontinuous.
 */
object P {

  /**
   * Convert the dynamic process to discontinuous.
   */
  def apply[A](m: Dynamics[A]): A @process = m.toProcess
}