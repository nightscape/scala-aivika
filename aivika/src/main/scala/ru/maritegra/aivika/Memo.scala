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
      arr(p.phase)(p.iteration) = x.apply(p)
    }

    def apply(p: Point): Double = {
      arr(p.phase)(p.iteration)
    }
  }

  private class FloatAcc(x: Dynamics[Float],
                         arr: Array[Array[Float]]) extends Acc {

    override def update(p: Point) {
      arr(p.phase)(p.iteration) = x.apply(p)
    }

    def apply(p: Point): Float = {
      arr(p.phase)(p.iteration)
    }
  }

  private class LongAcc(x: Dynamics[Long],
                        arr: Array[Array[Long]]) extends Acc {

    override def update(p: Point) {
      arr(p.phase)(p.iteration) = x.apply(p)
    }

    def apply(p: Point): Long = {
      arr(p.phase)(p.iteration)
    }
  }

  private class IntAcc(x: Dynamics[Int],
                       arr: Array[Array[Int]]) extends Acc {

    override def update(p: Point) {
      arr(p.phase)(p.iteration) = x.apply(p)
    }

    def apply(p: Point): Int = {
      arr(p.phase)(p.iteration)
    }
  }

  private class ShortAcc(x: Dynamics[Short],
                         arr: Array[Array[Short]]) extends Acc {

    override def update(p: Point) {
      arr(p.phase)(p.iteration) = x.apply(p)
    }

    def apply(p: Point): Short = {
      arr(p.phase)(p.iteration)
    }
  }

  private class ByteAcc(x: Dynamics[Byte],
                        arr: Array[Array[Byte]]) extends Acc {

    override def update(p: Point) {
      arr(p.phase)(p.iteration) = x.apply(p)
    }

    def apply(p: Point): Byte = {
      arr(p.phase)(p.iteration)
    }
  }

  private class BooleanAcc(x: Dynamics[Boolean],
                           arr: Array[Array[Boolean]]) extends Acc {

    override def update(p: Point) {
      arr(p.phase)(p.iteration) = x.apply(p)
    }

    def apply(p: Point): Boolean = {
      arr(p.phase)(p.iteration)
    }
  }

  private class CharAcc(x: Dynamics[Char],
                        arr: Array[Array[Char]]) extends Acc {

    override def update(p: Point) {
      arr(p.phase)(p.iteration) = x.apply(p)
    }

    def apply(p: Point): Char = {
      arr(p.phase)(p.iteration)
    }
  }

  private class UnitAcc(x: Dynamics[Unit]) extends Acc {

    override def update(p: Point) {
      x.apply(p)
    }

    def apply(p: Point): Unit = {
      // do nothing
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

    case Manifest.Unit =>
      memoUnit(x.asInstanceOf[Dynamics[Unit]]).asInstanceOf[Dynamics[A]]

    case _ =>
      memoGeneric(x)
  }

  def memoGeneric[A](x: Dynamics[A]): Dynamics[A] = {

    val f = new RunMemo((run: Run) => {

      val s = run.specs

      new AnyAcc(x, Array.ofDim(s.phases, s.iterations))
    })

    Interpolation.discrete(new Dynamics[A] {

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

    Interpolation.discrete(new Dynamics[Double] {

      def apply(p: Point): Double = {

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

    Interpolation.discrete(new Dynamics[Float] {

      def apply(p: Point): Float = {

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

    Interpolation.discrete(new Dynamics[Long] {

      def apply(p: Point): Long = {

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

    Interpolation.discrete(new Dynamics[Int] {

      def apply(p: Point): Int = {

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

    Interpolation.discrete(new Dynamics[Short] {

      def apply(p: Point): Short = {

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

    Interpolation.discrete(new Dynamics[Byte] {

      def apply(p: Point): Byte = {

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

    Interpolation.discrete(new Dynamics[Boolean] {

      def apply(p: Point): Boolean = {

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

    Interpolation.discrete(new Dynamics[Char] {

      def apply(p: Point): Char = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }

  def memoUnit(x: Dynamics[Unit]): Dynamics[Unit] = {

    val f = new RunMemo((run: Run) => {

      new UnitAcc(x)
    })

    Interpolation.discrete(new Dynamics[Unit] {

      def apply(p: Point): Unit = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }

  private abstract class Acc0 {

    private var iteration: Int = 0

    def update(p: Point)

    def sync(p: Point) {

      val n = p.iteration

      if (iteration < n) {

        val s = p.specs

        while (iteration < n) {

          val n2 = iteration
          val t2 = s.time(n2, 0)
          val p2 = Point(p.specs, p.run, t2, n2, 0)

          update(p2)

          if (n2 != iteration) sys.error("Recurrent loop.")

          iteration += 1
        }
      }
    }
  }

  private class AnyAcc0[A](x: Dynamics[A],
                           arr: Array[Any]) extends Acc0 {

    override def update(p: Point) {
      arr(p.iteration) = x.apply(p)
    }

    def apply(p: Point): A = {
      arr(p.iteration).asInstanceOf[A]
    }
  }

  private class DoubleAcc0(x: Dynamics[Double],
                           arr: Array[Double]) extends Acc0 {

    override def update(p: Point) {
      arr(p.iteration) = x.apply(p)
    }

    def apply(p: Point): Double = {
      arr(p.iteration)
    }
  }

  private class FloatAcc0(x: Dynamics[Float],
                          arr: Array[Float]) extends Acc0 {

    override def update(p: Point) {
      arr(p.iteration) = x.apply(p)
    }

    def apply(p: Point): Float = {
      arr(p.iteration)
    }
  }

  private class LongAcc0(x: Dynamics[Long],
                         arr: Array[Long]) extends Acc0 {

    override def update(p: Point) {
      arr(p.iteration) = x.apply(p)
    }

    def apply(p: Point): Long = {
      arr(p.iteration)
    }
  }

  private class IntAcc0(x: Dynamics[Int],
                        arr: Array[Int]) extends Acc0 {

    override def update(p: Point) {
      arr(p.iteration) = x.apply(p)
    }

    def apply(p: Point): Int = {
      arr(p.iteration)
    }
  }

  private class ShortAcc0(x: Dynamics[Short],
                          arr: Array[Short]) extends Acc0 {

    override def update(p: Point) {
      arr(p.iteration) = x.apply(p)
    }

    def apply(p: Point): Short = {
      arr(p.iteration)
    }
  }

  private class ByteAcc0(x: Dynamics[Byte],
                         arr: Array[Byte]) extends Acc0 {

    override def update(p: Point) {
      arr(p.iteration) = x.apply(p)
    }

    def apply(p: Point): Byte = {
      arr(p.iteration)
    }
  }

  private class BooleanAcc0(x: Dynamics[Boolean],
                            arr: Array[Boolean]) extends Acc0 {

    override def update(p: Point) {
      arr(p.iteration) = x.apply(p)
    }

    def apply(p: Point): Boolean = {
      arr(p.iteration)
    }
  }

  private class CharAcc0(x: Dynamics[Char],
                         arr: Array[Char]) extends Acc0 {

    override def update(p: Point) {
      arr(p.iteration) = x.apply(p)
    }

    def apply(p: Point): Char = {
      arr(p.iteration)
    }
  }

  private class UnitAcc0(x: Dynamics[Unit]) extends Acc0 {

    override def update(p: Point) {
      x.apply(p)
    }

    def apply(p: Point): Unit = {
      // do nothing
    }
  }

  def memo0[A](x: Dynamics[A])(implicit m: Manifest[A]): Dynamics[A] = m match {

    case Manifest.Double =>
      memoDouble0(x.asInstanceOf[Dynamics[Double]]).asInstanceOf[Dynamics[A]]

    case Manifest.Float =>
      memoFloat0(x.asInstanceOf[Dynamics[Float]]).asInstanceOf[Dynamics[A]]

    case Manifest.Long =>
      memoLong0(x.asInstanceOf[Dynamics[Long]]).asInstanceOf[Dynamics[A]]

    case Manifest.Int =>
      memoInt0(x.asInstanceOf[Dynamics[Int]]).asInstanceOf[Dynamics[A]]

    case Manifest.Short =>
      memoShort0(x.asInstanceOf[Dynamics[Short]]).asInstanceOf[Dynamics[A]]

    case Manifest.Byte =>
      memoByte0(x.asInstanceOf[Dynamics[Byte]]).asInstanceOf[Dynamics[A]]

    case Manifest.Char =>
      memoChar0(x.asInstanceOf[Dynamics[Char]]).asInstanceOf[Dynamics[A]]

    case Manifest.Unit =>
      memoUnit0(x.asInstanceOf[Dynamics[Unit]]).asInstanceOf[Dynamics[A]]

    case _ =>
      memoGeneric0(x)
  }

  def memoGeneric0[A](x: Dynamics[A]): Dynamics[A] = {

    val f = new RunMemo((run: Run) => {

      val s = run.specs

      new AnyAcc0(x, Array.ofDim(s.iterations))
    })

    Interpolation.discrete0(new Dynamics[A] {

      def apply(p: Point): A = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }

  def memoDouble0(x: Dynamics[Double]): Dynamics[Double] = {

    val f = new RunMemo((run: Run) => {

      val s = run.specs

      new DoubleAcc0(x, Array.ofDim(s.iterations))
    })

    Interpolation.discrete0(new Dynamics[Double] {

      def apply(p: Point): Double = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }

  def memoFloat0(x: Dynamics[Float]): Dynamics[Float] = {

    val f = new RunMemo((run: Run) => {

      val s = run.specs

      new FloatAcc0(x, Array.ofDim(s.iterations))
    })

    Interpolation.discrete0(new Dynamics[Float] {

      def apply(p: Point): Float = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }

  def memoLong0(x: Dynamics[Long]): Dynamics[Long] = {

    val f = new RunMemo((run: Run) => {

      val s = run.specs

      new LongAcc0(x, Array.ofDim(s.iterations))
    })

    Interpolation.discrete0(new Dynamics[Long] {

      def apply(p: Point): Long = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }

  def memoInt0(x: Dynamics[Int]): Dynamics[Int] = {

    val f = new RunMemo((run: Run) => {

      val s = run.specs

      new IntAcc0(x, Array.ofDim(s.iterations))
    })

    Interpolation.discrete0(new Dynamics[Int] {

      def apply(p: Point): Int = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }

  def memoShort0(x: Dynamics[Short]): Dynamics[Short] = {

    val f = new RunMemo((run: Run) => {

      val s = run.specs

      new ShortAcc0(x, Array.ofDim(s.iterations))
    })

    Interpolation.discrete0(new Dynamics[Short] {

      def apply(p: Point): Short = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }

  def memoByte0(x: Dynamics[Byte]): Dynamics[Byte] = {

    val f = new RunMemo((run: Run) => {

      val s = run.specs

      new ByteAcc0(x, Array.ofDim(s.iterations))
    })

    Interpolation.discrete0(new Dynamics[Byte] {

      def apply(p: Point): Byte = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }

  def memoBoolean0(x: Dynamics[Boolean]): Dynamics[Boolean] = {

    val f = new RunMemo((run: Run) => {

      val s = run.specs

      new BooleanAcc0(x, Array.ofDim(s.iterations))
    })

    Interpolation.discrete0(new Dynamics[Boolean] {

      def apply(p: Point): Boolean = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }

  def memoChar0(x: Dynamics[Char]): Dynamics[Char] = {

    val f = new RunMemo((run: Run) => {

      val s = run.specs

      new CharAcc0(x, Array.ofDim(s.iterations))
    })

    Interpolation.discrete0(new Dynamics[Char] {

      def apply(p: Point): Char = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }

  def memoUnit0(x: Dynamics[Unit]): Dynamics[Unit] = {

    val f = new RunMemo((run: Run) => {

      new UnitAcc0(x)
    })

    Interpolation.discrete0(new Dynamics[Unit] {

      def apply(p: Point): Unit = {

        val acc = f(p.run)

        acc.sync(p)
        acc.apply(p)
      }
    })
  }
}
