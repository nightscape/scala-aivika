/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.util

trait Event[+A] {

  def addListener(h: (Any, A) => Unit)

  def removeListener(h: (Any, A) => Unit)

  def add(h: A => Unit) = addListener((sender: Any, a: A) => h(a))
}

object Event {

  implicit def event2observable[A](e: Event[A]): Observable[A] = new Observable[A] {

    def subscribe(h: A => Unit) = {

      val l = (sender: Any, a: A) => h(a)

      e.addListener(l)

      new Disposable {
        def dispose() = e.removeListener(l)
      }
    }
  }
}