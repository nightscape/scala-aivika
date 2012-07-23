/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

object Interpolation {

  def discrete[T](x: Dynamics[T]): Dynamics[T] = new Dynamics[T] {

    def apply(p: Point): T = {

      if (p.phase >= 0) x.apply(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.apply(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }

    override def applyForDouble(p: Point): Double = {

      if (p.phase >= 0) x.applyForDouble(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.applyForDouble(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }

    override def applyForFloat(p: Point): Float = {

      if (p.phase >= 0) x.applyForFloat(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.applyForFloat(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }

    override def applyForLong(p: Point): Long = {

      if (p.phase >= 0) x.applyForLong(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.applyForLong(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }

    override def applyForInt(p: Point): Int = {

      if (p.phase >= 0) x.applyForInt(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.applyForInt(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }

    override def applyForShort(p: Point): Short = {

      if (p.phase >= 0) x.applyForShort(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.applyForShort(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }

    override def applyForByte(p: Point): Byte = {

      if (p.phase >= 0) x.applyForByte(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.applyForByte(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }

    override def applyForBoolean(p: Point): Boolean = {

      if (p.phase >= 0) x.applyForBoolean(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.applyForBoolean(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }

    override def applyForChar(p: Point): Char = {

      if (p.phase >= 0) x.applyForChar(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.applyForChar(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }
  }

  def discrete0[T](x: Dynamics[T]): Dynamics[T] = new Dynamics[T] {

    def apply(p: Point): T = {

      if (p.phase == 0) x.apply(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.apply(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }

    override def applyForDouble(p: Point): Double = {

      if (p.phase == 0) x.applyForDouble(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.applyForDouble(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }

    override def applyForFloat(p: Point): Float = {

      if (p.phase == 0) x.applyForFloat(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.applyForFloat(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }

    override def applyForLong(p: Point): Long = {

      if (p.phase == 0) x.applyForLong(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.applyForLong(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }

    override def applyForInt(p: Point): Int = {

      if (p.phase == 0) x.applyForInt(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.applyForInt(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }

    override def applyForShort(p: Point): Short = {

      if (p.phase == 0) x.applyForShort(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.applyForShort(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }

    override def applyForByte(p: Point): Byte = {

      if (p.phase == 0) x.applyForByte(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.applyForByte(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }

    override def applyForBoolean(p: Point): Boolean = {

      if (p.phase == 0) x.applyForBoolean(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.applyForBoolean(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }

    override def applyForChar(p: Point): Char = {

      if (p.phase == 0) x.applyForChar(p)
      else {

        val s = p.specs
        val n = p.iteration

        x.applyForChar(Point(p.specs, p.run, s.time(n, 0), n, 0))
      }
    }
  }
}
