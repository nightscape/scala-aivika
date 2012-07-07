/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.util

import scala.collection.immutable.Set

class EventSource[A] {

  private var listeners: Set[(Any, A) => Unit] = null

  def trigger(sender: Any, message: A) {

    val hs = listeners

    if (hs != null) {
      for (h <- hs) h(sender, message)
    }
  }

  private def addListener(h: (Any, A) => Unit) = synchronized {

    if (listeners == null) {
      listeners = Set(h)

    } else {
      listeners += h
    }
  }

  private def removeListener(h: (Any, A) => Unit) = synchronized {

    if (listeners != null) {

      listeners -= h

      if (listeners.isEmpty) {
        listeners = null
      }
    }
  }

  def publish: Event[A] = new Event[A] {

    def addListener(h: (Any, A) => Unit) = EventSource.this.addListener(h)

    def removeListener(h: (Any, A) => Unit) = EventSource.this.removeListener(h)
  }
}