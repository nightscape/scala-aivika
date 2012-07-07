/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

import org.scalatest.junit.JUnit3Suite

class ChemicalReactionTestCase extends JUnit3Suite {

  val epsilon = 0.0000001

  def testEuler() {

    val s = Specs(0, 1, 0.1, Euler)
    val c = ChemicalReaction.c.run1(s)
    
    assert(math.abs(c - 26.390107090000008) < epsilon)
  }

  def testRK2() {

    val s = Specs(0, 1, 0.1, RungeKutta2)
    val c = ChemicalReaction.c.run1(s)
    
    assert(math.abs(c - 26.495416837065086) < epsilon)
  }

  def testRK4() {

    val s = Specs(0, 1, 0.1, RungeKutta4)
    val c = ChemicalReaction.c.run1(s)
    
    assert(math.abs(c - 26.424214521663316) < epsilon)
  }
}
