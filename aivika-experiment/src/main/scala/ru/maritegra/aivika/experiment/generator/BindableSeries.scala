/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment.generator

import scala.xml._
import scala.collection.mutable.{Buffer, ArrayBuffer, Map}

import ru.maritegra.aivika._
import ru.maritegra.aivika.util._
import ru.maritegra.aivika.experiment._

private trait BindableSeries[A] {

  val experiment: Experiment
  val series: SeriesBuffer[A]

  protected type RunRep
  protected type DynamicsRep
  protected type DynamicsSeqRep
  protected type DynamicsSeq2Rep
  protected type DynamicsSeq3Rep
  protected type DynamicsSeq4Rep
  protected type DynamicsSeq5Rep

  private sealed abstract class Binding
  private case class DynamicsBinding(series: DynamicsSeries[A], binding: DynamicsRep) extends Binding
  private case class DynamicsSeqBinding(series: DynamicsSeqSeries[A], binding: DynamicsSeqRep) extends Binding
  private case class DynamicsSeq2Binding(series: DynamicsSeq2Series[A], binding: DynamicsSeq2Rep) extends Binding
  private case class DynamicsSeq3Binding(series: DynamicsSeq3Series[A], binding: DynamicsSeq3Rep) extends Binding
  private case class DynamicsSeq4Binding(series: DynamicsSeq4Series[A], binding: DynamicsSeq4Rep) extends Binding
  private case class DynamicsSeq5Binding(series: DynamicsSeq5Series[A], binding: DynamicsSeq5Rep) extends Binding

  private val bindings = ArrayBuffer[(RunRep, Map[Series[A], Binding])]()

  protected def init() {

    for (runIndex <- 0 to experiment.runCount - 1) {

      val run = initRun(runIndex)
      val bs = Map[Series[A], Binding]()

      for (s <- series.buffer) {

        val binding = s match {

          case s @ DynamicsSeries(_, _) =>
            DynamicsBinding(s, initSeries(run, s))

          case s @ DynamicsSeqSeries(_, _) =>
            DynamicsSeqBinding(s, initSeriesSeq(run, s))

          case s @ DynamicsSeq2Series(_, _) =>
            DynamicsSeq2Binding(s, initSeriesSeq2(run, s))

          case s @ DynamicsSeq3Series(_, _) =>
            DynamicsSeq3Binding(s, initSeriesSeq3(run, s))

          case s @ DynamicsSeq4Series(_, _) =>
            DynamicsSeq4Binding(s, initSeriesSeq4(run, s))

          case s @ DynamicsSeq5Series(_, _) =>
            DynamicsSeq5Binding(s, initSeriesSeq5(run, s))
        }

        bs += s -> binding
      }

      bindings += ((run, bs))
    }
  }

  protected def initSeries(run: RunRep, series: DynamicsSeries[A]): DynamicsRep = {
    initSeries(run, series.name, series.data)
  }

  protected def initSeriesSeq(run: RunRep, series: DynamicsSeqSeries[A]): DynamicsSeqRep = {
    initSeriesSeq(run, series.name, series.data)
  }

  protected def initSeriesSeq2(run: RunRep, series: DynamicsSeq2Series[A]): DynamicsSeq2Rep = {
    initSeriesSeq2(run, series.name, series.data)
  }

  protected def initSeriesSeq3(run: RunRep, series: DynamicsSeq3Series[A]): DynamicsSeq3Rep = {
    initSeriesSeq3(run, series.name, series.data)
  }

  protected def initSeriesSeq4(run: RunRep, series: DynamicsSeq4Series[A]): DynamicsSeq4Rep = {
    initSeriesSeq4(run, series.name, series.data)
  }

  protected def initSeriesSeq5(run: RunRep, series: DynamicsSeq5Series[A]): DynamicsSeq5Rep = {
    initSeriesSeq5(run, series.name, series.data)
  }

  protected def initRun(runIndex: Int): RunRep
  protected def initSeries(run: RunRep, name: String, data: Dynamics[A]): DynamicsRep
  protected def initSeriesSeq(run: RunRep, name: String, data: Seq[Dynamics[A]]): DynamicsSeqRep
  protected def initSeriesSeq2(run: RunRep, name: String, data: Seq[Seq[Dynamics[A]]]): DynamicsSeq2Rep
  protected def initSeriesSeq3(run: RunRep, name: String, data: Seq[Seq[Seq[Dynamics[A]]]]): DynamicsSeq3Rep
  protected def initSeriesSeq4(run: RunRep, name: String, data: Seq[Seq[Seq[Seq[Dynamics[A]]]]]): DynamicsSeq4Rep
  protected def initSeriesSeq5(run: RunRep, name: String, data: Seq[Seq[Seq[Seq[Seq[Dynamics[A]]]]]]): DynamicsSeq5Rep

  def run(): Disposable = {

    val hs = ArrayBuffer[Disposable]()

    for (runIndex <- 0 to experiment.runCount - 1) {

      val (run, bs) = bindings(runIndex)

      hs += startRun(run)

      for ((k, v) <- bs) {

        v match {

          case v @ DynamicsBinding(_, _) =>
            hs += startSeries(run, v.series, v.binding)

          case v @ DynamicsSeqBinding(_, _) =>
            hs += startSeriesSeq(run, v.series, v.binding)

          case v @ DynamicsSeq2Binding(_, _) =>
            hs += startSeriesSeq2(run, v.series, v.binding)

          case v @ DynamicsSeq3Binding(_, _) =>
            hs += startSeriesSeq3(run, v.series, v.binding)

          case v @ DynamicsSeq4Binding(_, _) =>
            hs += startSeriesSeq4(run, v.series, v.binding)

          case v @ DynamicsSeq5Binding(_, _) =>
            hs += startSeriesSeq5(run, v.series, v.binding)
        }
      }
    }

    new Disposable {

      def dispose() {

        for (h <- hs) {
          h.dispose()
        }
      }
    }
  }

  protected def startSeries(run: RunRep, series: DynamicsSeries[A], binding: DynamicsRep): Disposable = {
    startSeries(run, series.name, series.data, binding)
  }

  protected def startSeriesSeq(run: RunRep, series: DynamicsSeqSeries[A], binding: DynamicsSeqRep): Disposable = {
    startSeriesSeq(run, series.name, series.data, binding)
  }

  protected def startSeriesSeq2(run: RunRep, series: DynamicsSeq2Series[A], binding: DynamicsSeq2Rep): Disposable = {
    startSeriesSeq2(run, series.name, series.data, binding)
  }

  protected def startSeriesSeq3(run: RunRep, series: DynamicsSeq3Series[A], binding: DynamicsSeq3Rep): Disposable = {
    startSeriesSeq3(run, series.name, series.data, binding)
  }

  protected def startSeriesSeq4(run: RunRep, series: DynamicsSeq4Series[A], binding: DynamicsSeq4Rep): Disposable = {
    startSeriesSeq4(run, series.name, series.data, binding)
  }

  protected def startSeriesSeq5(run: RunRep, series: DynamicsSeq5Series[A], binding: DynamicsSeq5Rep): Disposable = {
    startSeriesSeq5(run, series.name, series.data, binding)
  }

  protected def startRun(run: RunRep): Disposable
  protected def startSeries(run: RunRep, name: String, data: Dynamics[A], binding: DynamicsRep): Disposable
  protected def startSeriesSeq(run: RunRep, name: String, data: Seq[Dynamics[A]], binding: DynamicsSeqRep): Disposable
  protected def startSeriesSeq2(run: RunRep, name: String, data: Seq[Seq[Dynamics[A]]], binding: DynamicsSeq2Rep): Disposable
  protected def startSeriesSeq3(run: RunRep, name: String, data: Seq[Seq[Seq[Dynamics[A]]]], binding: DynamicsSeq3Rep): Disposable
  protected def startSeriesSeq4(run: RunRep, name: String, data: Seq[Seq[Seq[Seq[Dynamics[A]]]]], binding: DynamicsSeq4Rep): Disposable
  protected def startSeriesSeq5(run: RunRep, name: String, data: Seq[Seq[Seq[Seq[Seq[Dynamics[A]]]]]], binding: DynamicsSeq5Rep): Disposable

  def toHtml: Seq[Node] = toHtml(bindings map (_._1))

  protected def toHtml(runs: Buffer[RunRep]): Seq[Node]
}
