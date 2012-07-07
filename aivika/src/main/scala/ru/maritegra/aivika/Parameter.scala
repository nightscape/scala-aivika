/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

class Parameter[A](f: Int => A) extends Dynamics[A] {
  
  def this(f: () => A) = this(i => f())
  
  def this(xs: Seq[A]) = this(i => xs(i % xs.length))

  private def initSynchronously(run: Run): A = run.simulation match {

    case Some(x) => x.synchronized { f(run.index) }
    case _ => f(run.index)
  }

  private class State(var value: Option[A])

  private val state = new RunMemo((run: Run) => new State(None))

  def apply(p: Point): A = {
    
    val s = state(p.run)
    
    s.value match {

      case Some(x) => x
      case None =>

        val x = initSynchronously(p.run)
        s.value = Some(x)

        x
    }
  }
}
