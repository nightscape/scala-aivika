/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment

import java.io._

sealed abstract class ExperimentDirectory {

  def toFile(replacement: Map[String, String] = Map()): File = {

    var x = this match {

      case WritableDirectory(x) => x
      case UniqueDirectory(x) => x
    }
    
    for ((k, v) <- replacement) {
      x = x.replace(k, v)
    }

    this match {

      case WritableDirectory(_) => new File(x)
      case UniqueDirectory(_) =>

        var f = new File(x)
        var i = 2

        while (f.exists()) {

          f = new File(x + "(" + i + ")")
          i = i + 1
        }

        f
    }
  }
}

case class UniqueDirectory(name: String) extends ExperimentDirectory
case class WritableDirectory(name: String) extends ExperimentDirectory

