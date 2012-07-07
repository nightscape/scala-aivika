/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

/**
 * Specifies the simulation time point.
 */
case class Point private[aivika]
  (specs: Specs, run: Run, time: Double, iteration: Int, phase: Int)
