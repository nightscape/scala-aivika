/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

case class Specs(starttime: Double,
                 stoptime: Double,
                 dt: Double,
                 method: Method) {

  def timeDelta(phase: Int): Double = method match {

    case Euler => phase match {
      
      case 0 => 0.0
    }

    case RungeKutta2 => phase match {
      
      case 0 => 0.0
      case 1 => dt
    }
    
    case RungeKutta4 => phase match {

      case 0 => 0.0
      case 1 => dt / 2.0
      case 2 => dt / 2.0
      case 3 => dt
    }
  }

  def time(iteration: Int, phase: Int): Double =
    starttime + iteration * dt + timeDelta(phase)

  lazy val iterations: Int =
    1 + (math.ceil ((stoptime - starttime) / dt).asInstanceOf[Int])

  lazy val phases: Int = method match {

    case Euler => 1
    case RungeKutta2 => 2
    case RungeKutta4 => 4
  }
}
