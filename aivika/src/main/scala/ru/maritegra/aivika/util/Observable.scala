/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.util

import scala.util.continuations._

trait Observable[+A] { outer =>

  def subscribe(h: A => Unit): Disposable

  def map[B](f: A => B): Observable[B] = new Observable[B] {

    def subscribe(h: B => Unit) = {
      outer.subscribe(a => h(f(a)))
    }
  }

  def filter(p: A => Boolean): Observable[A] = new Observable[A] {

    def subscribe(h: A => Unit) = {
      outer.subscribe(a => if (p(a)) h(a))
    }
  }

  def collect[B](f: PartialFunction[A, B]): Observable[B] = new Observable[B] {

    def subscribe(h: B => Unit) = {
      outer.subscribe(a => if (f.isDefinedAt(a)) h(f(a)))
    }
  }

  def merge[A1 >: A](that: Observable[A1]): Observable[A1] = new Observable[A1] {

    def subscribe(h: A1 => Unit) = {

      val d1 = outer.subscribe(a => h(a))
      val d2 = that.subscribe(a => h(a))

      new Disposable {

        def dispose() {

          d1.dispose()
          d2.dispose()
        }
      }
    }
  }

  def partition(p: A => Boolean): (Observable[A], Observable[A]) =
    (filter (p), filter (! p(_)))

  def choose[B](f: A => Option[B]): Observable[B] = new Observable[B] {

    def subscribe(h: B => Unit) = {

      outer.subscribe(a => f(a) match {

        case Some (b) => h(b)
        case None     =>
      })
    }
  }

  def pairwise(): Observable[(A, A)] = new Observable[(A, A)] {

    def subscribe(h: ((A, A)) => Unit) = {

      var s: Option[A] = None

      outer.subscribe(a => s match {

        case Some (b) => { h(b, a); s = Some (a) }
        case None     => { s = Some (a) }
      })
    }
  }

  def scan[B](z:B)(f: (B, A) => B): Observable[B] = new Observable[B] {

    def subscribe(h: B => Unit) = {

      var s = z

      outer.subscribe(a => { s = f (s, a); h(s) })
    }
  }

  def await(): A @suspendable = shift {
    k: (A => Unit) =>

      var d1: Disposable = null
      val d2: Disposable = subscribe(a => {

        if (d1 != null) d1.dispose()
        k(a)
      })

      d1 = d2
  }

  def once(h: A => Unit): Disposable = {

    var d0: Disposable = null

    val d1 = new Disposable {

      def dispose() {

        if (d0 != null) {

          d0.dispose()
          d0 = null
        }
      }
    }

    val d2 = subscribe(a => {

      d1.dispose()
      h(a)
    })

    d0 = d2
    d1
  }
}