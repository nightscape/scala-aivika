/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

object Memo {

  private abstract class Acc {

    private var iteration: Int = 0
    private var phase: Int = 0

    def update(p: Point)

    def sync(p: Point) {

      val n = p.iteration
      val ph = p.phase

      if ((iteration < n) || (iteration == n) && (phase <= ph)) {
	
        val s = p.specs
        val phs = s.phases

        while ((iteration < n) || (iteration == n) && (phase <= ph)) {

          val n2 = iteration
          val ph2 = phase
          val t2 = s.time(n2, ph2)
          val p2 = Point(p.specs, p.run, t2, n2, ph2)

          update(p2)

          if (n2 != iteration) sys.error("Recurrent loop.")
          if (ph2 != phase) sys.error("Recurrent loop.")

          phase += 1

          if (phase >= phs) {

            phase = 0
            iteration += 1
          }
        }
      }
    }
  }

  private class AnyAcc[A](x: Dynamics[A],
                          arr: Array[Array[Any]]) extends Acc {

    override def update(p: Point) {
      arr(p.phase)(p.iteration) = x.apply(p)
    }

    def apply(p: Point): A = {
      arr(p.phase)(p.iteration).asInstanceOf[A]
    }
  }

  private class DoubleAcc(x: Dynamics[Double],
                          arr: Array[Array[Double]]) extends Acc {

    override def update(p: Point) {
      arr(p.phase)(p.iteration) = x.applyForDouble(p)
    }

    def apply(p: Point): Double = {
      arr(p.phase)(p.iteration)
    }
  }

  private class FloatAcc(x: Dynamics[Float],
                         arr: Array[Array[Float]]) extends Acc {

    override def update(p: Point) {
      arr(p.phase)(p.iteration) = x.applyForFloat(p)
    }

    def apply(p: Point): Float = {
      arr(p.phase)(p.iteration)
    }
  }

  private class LongAcc(x: Dynamics[Long],
                        arr: Array[Array[Long]]) extends Acc {

    override def update(p: Point) {
      arr(p.phase)(p.iteration) = x.applyForLong(p)
    }

    def apply(p: Point): Long = {
      arr(p.phase)(p.iteration)
    }
  }

  private class IntAcc(x: Dynamics[Int],
                       arr: Array[Array[Int]]) extends Acc {

    override def update(p: Point) {
      arr(p.phase)(p.iteration) = x.applyForInt(p)
    }

    def apply(p: Point): Int = {
      arr(p.phase)(p.iteration)
    }
  }

  private class ShortAcc(x: Dynamics[Short],
                         arr: Array[Array[Short]]) extends Acc {

    override def update(p: Point) {
      arr(p.phase)(p.iteration) = x.applyForShort(p)
    }

    def apply(p: Point): Short = {
      arr(p.phase)(p.iteration)
    }
  }

  private class ByteAcc(x: Dynamics[Byte],
                        arr: Array[Array[Byte]]) extends Acc {

    override def update(p: Point) {
      arr(p.phase)(p.iteration) = x.applyForByte(p)
    }

    def apply(p: Point): Byte = {
      arr(p.phase)(p.iteration)
    }
  }

  private class BooleanAcc(x: Dynamics[Boolean],
                           arr: Array[Array[Boolean]]) extends Acc {

    override def update(p: Point) {
      arr(p.phase)(p.iteration) = x.applyForBoolean(p)
    }

    def apply(p: Point): Boolean = {
      arr(p.phase)(p.iteration)
    }
  }

  private class CharAcc(x: Dynamics[Char],
                        arr: Array[Array[Char]]) extends Acc {

    override def update(p: Point) {
      arr(p.phase)(p.iteration) = x.applyForChar(p)
    }

    def apply(p: Point): Char = {
      arr(p.phase)(p.iteration)
    }
  }

  def memo[A](x: Dynamics[A])(implicit m: Manifest[A]): Dynamics[A] = m match {

    case Manifest.Double =>
      memoDouble(x.asInstanceOf[Dynamics[Double]]).asInstanceOf[Dynamics[A]]

    case Manifest.Float =>
      memoFloat(x.asInstanceOf[Dynamics[Float]]).asInstanceOf[Dynamics[A]]

    case Manifest.Long =>
      memoLong(x.asInstanceOf[Dynamics[Long]]).asInstanceOf[Dynamics[A]]

    case Manifest.Int =>
      memoInt(x.asInstanceOf[Dynamics[Int]]).asInstanceOf[Dynamics[A]]

    case Manifest.Short =>
      memoShort(x.asInstanceOf[Dynamics[Short]]).asInstanceOf[Dynamics[A]]

    case Manifest.Byte =>
      memoByte(x.asInstanceOf[Dynamics[Byte]]).asInstanceOf[Dynamics[A]]

    case Manifest.Char =>
      memoChar(x.asInstanceOf[Dynamics[Char]]).asInstanceOf[Dynamics[A]]

    case _ =>
      memoGeneric(x)
  }

  def memoGeneric[A](x: Dynamics[A]): Dynamics[A] = {

    val f = new RunMemo((run: Run) => {

      val s = run.specs

      new AnyAcc(x, Array.ofDim(s.phases, s.iterations))
    })

    Interpolation.discrete4(new Dynamics[A] {

      def apply(p: Point): A = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }

  def memoDouble(x: Dynamics[Double]): Dynamics[Double] = {

    val f = new RunMemo((run: Run) => {

      val s = run.specs

      new DoubleAcc(x, Array.ofDim(s.phases, s.iterations))
    })

    Interpolation.discrete4(new Dynamics[Double] {

      def apply(p: Point): Double = applyForDouble(p)

      override def applyForDouble(p: Point): Double = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }

  def memoFloat(x: Dynamics[Float]): Dynamics[Float] = {

    val f = new RunMemo((run: Run) => {

      val s = run.specs

      new FloatAcc(x, Array.ofDim(s.phases, s.iterations))
    })

    Interpolation.discrete4(new Dynamics[Float] {

      def apply(p: Point): Float = applyForFloat(p)

      override def applyForDouble(p: Point): Double = applyForFloat(p)

      override def applyForFloat(p: Point): Float = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }

  def memoLong(x: Dynamics[Long]): Dynamics[Long] = {

    val f = new RunMemo((run: Run) => {

      val s = run.specs

      new LongAcc(x, Array.ofDim(s.phases, s.iterations))
    })

    Interpolation.discrete4(new Dynamics[Long] {

      def apply(p: Point): Long = applyForLong(p)

      override def applyForDouble(p: Point): Double = applyForLong(p)

      override def applyForFloat(p: Point): Float = applyForLong(p)

      override def applyForLong(p: Point): Long = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }

  def memoInt(x: Dynamics[Int]): Dynamics[Int] = {

    val f = new RunMemo((run: Run) => {

      val s = run.specs

      new IntAcc(x, Array.ofDim(s.phases, s.iterations))
    })

    Interpolation.discrete4(new Dynamics[Int] {

      def apply(p: Point): Int = applyForInt(p)

      override def applyForDouble(p: Point): Double = applyForInt(p)

      override def applyForFloat(p: Point): Float = applyForInt(p)

      override def applyForLong(p: Point): Long = applyForInt(p)

      override def applyForInt(p: Point): Int = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }

  def memoShort(x: Dynamics[Short]): Dynamics[Short] = {

    val f = new RunMemo((run: Run) => {

      val s = run.specs

      new ShortAcc(x, Array.ofDim(s.phases, s.iterations))
    })

    Interpolation.discrete4(new Dynamics[Short] {

      def apply(p: Point): Short = applyForShort(p)

      override def applyForDouble(p: Point): Double = applyForShort(p)

      override def applyForFloat(p: Point): Float = applyForShort(p)

      override def applyForLong(p: Point): Long = applyForShort(p)

      override def applyForInt(p: Point): Int = applyForShort(p)

      override def applyForShort(p: Point): Short = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }

  def memoByte(x: Dynamics[Byte]): Dynamics[Byte] = {

    val f = new RunMemo((run: Run) => {

      val s = run.specs

      new ByteAcc(x, Array.ofDim(s.phases, s.iterations))
    })

    Interpolation.discrete4(new Dynamics[Byte] {

      def apply(p: Point): Byte = applyForByte(p)

      override def applyForDouble(p: Point): Double = applyForByte(p)

      override def applyForFloat(p: Point): Float = applyForByte(p)

      override def applyForLong(p: Point): Long = applyForByte(p)

      override def applyForInt(p: Point): Int = applyForByte(p)

      override def applyForShort(p: Point): Short = applyForByte(p)

      override def applyForByte(p: Point): Byte = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }

  def memoBoolean(x: Dynamics[Boolean]): Dynamics[Boolean] = {

    val f = new RunMemo((run: Run) => {

      val s = run.specs

      new BooleanAcc(x, Array.ofDim(s.phases, s.iterations))
    })

    Interpolation.discrete4(new Dynamics[Boolean] {

      def apply(p: Point): Boolean = applyForBoolean(p)

      override def applyForBoolean(p: Point): Boolean = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }

  def memoChar(x: Dynamics[Char]): Dynamics[Char] = {

    val f = new RunMemo((run: Run) => {

      val s = run.specs

      new CharAcc(x, Array.ofDim(s.phases, s.iterations))
    })

    Interpolation.discrete4(new Dynamics[Char] {

      def apply(p: Point): Char = applyForChar(p)

      override def applyForChar(p: Point): Char = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }
}
