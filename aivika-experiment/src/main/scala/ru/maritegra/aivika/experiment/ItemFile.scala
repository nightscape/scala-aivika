/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment

import java.io.File

sealed abstract class ItemFile {

  def toFile(parent: File, replacement: Map[String, String] = Map()): File = {

    var x = this match {

      case WritableFile(x, _) => x
      case UniqueFile(x, _) => x
    }

    for ((k, v) <- replacement) {
      x = x.replace(k, v)
    }

    this match {

      case WritableFile(_, y) => new File(parent, x + y)
      case UniqueFile(_, y) =>

        var f = new File(parent, x + y)
        var i = 2

        while (f.exists()) {

          f = new File(parent, x + "(" + i + ")" + y)
          i = i + 1
        }

        f
    }
  }
}

case class UniqueFile(name: String, extension: String) extends ItemFile
case class WritableFile(name: String, extension: String) extends ItemFile
