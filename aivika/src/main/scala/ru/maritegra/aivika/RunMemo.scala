/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

import scala.collection.immutable.Map

class RunMemo[T](f: Run => T) extends (Run => T) {

  private var m: Map[Run, T] = Map()

  def apply(run: Run): T = {

    if (m.contains(run)) {
      m(run)

    } else if (run.isDisposed()) {
      sys.error("The run is disposed.")
      
    } else {

      val t = f(run)

      add(run, t)

      run.disposed add (unit => remove(run))

      t
    }
  }

  private def add(run: Run, t: T) {

    synchronized {
      m += run -> t
    }
  }

  private def remove(run: Run) {

    synchronized {
      m -= run
    }
  }
}
