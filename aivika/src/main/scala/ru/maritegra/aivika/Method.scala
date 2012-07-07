/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

abstract sealed class Method

case object Euler extends Method
case object RungeKutta2 extends Method
case object RungeKutta4 extends Method
