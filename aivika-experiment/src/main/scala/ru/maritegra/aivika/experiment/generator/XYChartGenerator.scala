/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment.generator

import java.io._

import scala.xml._
import scala.collection.mutable.Buffer

import org.jfree.chart.{JFreeChart, ChartFactory, ChartUtilities}
import org.jfree.chart.axis.{AxisLocation, NumberAxis}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.chart.renderer.xy.{StandardXYItemRenderer, XYDotRenderer, XYStepRenderer, XYAreaRenderer}

import ru.maritegra.aivika._
import ru.maritegra.aivika.util._
import ru.maritegra.aivika.experiment._

private class XYChartGenerator(parent: ExperimentGenerator, item: XYChartItem, id: Int)
  extends ItemGenerator(parent, item) with BindableSeries[Double] {

  val experiment = parent.experiment
  val series = item.value.ys

  protected class RunBinding(val index: Int,
                             val vars: Buffer[Dynamics[Double]],
                             val names: Buffer[String],
                             val data: Buffer[XYSeries],
                             var file: File)

  protected class DynamicsBinding()
  protected class DynamicsSeqBinding()
  protected class DynamicsSeq2Binding()
  protected class DynamicsSeq3Binding()
  protected class DynamicsSeq4Binding()
  protected class DynamicsSeq5Binding()

  protected type RunRep = RunBinding

  protected type DynamicsRep = DynamicsBinding
  protected type DynamicsSeqRep = DynamicsSeqBinding
  protected type DynamicsSeq2Rep = DynamicsSeq2Binding
  protected type DynamicsSeq3Rep = DynamicsSeq3Binding
  protected type DynamicsSeq4Rep = DynamicsSeq4Binding
  protected type DynamicsSeq5Rep = DynamicsSeq5Binding

  init()

  protected def initRun(runIndex: Int): RunBinding = {
    new RunBinding(runIndex, Buffer(), Buffer(), Buffer(), null)
  }

  protected def initSeries(run: RunBinding, name: String, data: Dynamics[Double]): DynamicsBinding = {

    run.vars += data
    run.names += name

    new DynamicsBinding()
  }

  protected def initSeriesSeq(run: RunBinding, name: String, data: Seq[Dynamics[Double]]): DynamicsSeqBinding = {

    for (i <- 0 to data.length - 1) {

      run.vars += data(i)
      run.names += (name + "(" + i + ")")
    }

    new DynamicsSeqBinding()
  }

  protected def initSeriesSeq2(run: RunBinding, name: String, data: Seq[Seq[Dynamics[Double]]]): DynamicsSeq2Binding = {

    for (i1 <- 0 to data.length - 1) {
      for (i2 <- 0 to data(i1).length - 1) {

        run.vars += data(i1)(i2)
        run.names += (name + "(" + i1 + ")(" + i2 + ")")
      }
    }

    new DynamicsSeq2Binding()
  }

  protected def initSeriesSeq3(run: RunBinding, name: String, data: Seq[Seq[Seq[Dynamics[Double]]]]): DynamicsSeq3Binding = {

    for (i1 <- 0 to data.length - 1) {
      for (i2 <- 0 to data(i1).length - 1) {
        for (i3 <- 0 to data(i1)(i2).length - 1) {

          run.vars += data(i1)(i2)(i3)
          run.names += (name + "(" + i1 + ")(" + i2 + ")(" + i3 + ")")
        }
      }
    }

    new DynamicsSeq3Binding()
  }

  protected def initSeriesSeq4(run: RunBinding, name: String, data: Seq[Seq[Seq[Seq[Dynamics[Double]]]]]): DynamicsSeq4Binding = {

    for (i1 <- 0 to data.length - 1) {
      for (i2 <- 0 to data(i1).length - 1) {
        for (i3 <- 0 to data(i1)(i2).length - 1) {
          for (i4 <- 0 to data(i1)(i2)(i3).length - 1) {

            run.vars += data(i1)(i2)(i3)(i4)
            run.names += (name + "(" + i1 + ")(" + i2 + ")(" + i3 + ")(" + i4 + ")")
          }
        }
      }
    }

    new DynamicsSeq4Binding()
  }

  protected def initSeriesSeq5(run: RunBinding, name: String, data: Seq[Seq[Seq[Seq[Seq[Dynamics[Double]]]]]]): DynamicsSeq5Binding = {

    for (i1 <- 0 to data.length - 1) {
      for (i2 <- 0 to data(i1).length - 1) {
        for (i3 <- 0 to data(i1)(i2).length - 1) {
          for (i4 <- 0 to data(i1)(i2)(i3).length - 1) {
            for (i5 <- 0 to data(i1)(i2)(i3)(i4).length - 1) {

              run.vars += data(i1)(i2)(i3)(i4)(i5)
              run.names += (name + "(" + i1 + ")(" + i2 + ")(" + i3 + ")(" + i4 + ")(" + i5 + ")")
            }
          }
        }
      }
    }

    new DynamicsSeq5Binding()
  }

  protected def startRun(run: RunBinding): Disposable = {

    val hs = Buffer[Disposable]()

    hs += processRunStarted(run)
    hs += processIntegPoint(run)
    hs += processRunFinished(run)

    new Disposable {

      def dispose() {

        for (h <- hs) {
          h.dispose()
        }
      }
    }
  }

  protected def startSeries(run: RunBinding, name: String, data: Dynamics[Double],
                            binding: DynamicsBinding) = new Disposable { def dispose() {} }

  protected def startSeriesSeq(run: RunBinding, name: String, data: Seq[Dynamics[Double]],
                               binding: DynamicsSeqBinding) = new Disposable { def dispose() {} }

  protected def startSeriesSeq2(run: RunBinding, name: String, data: Seq[Seq[Dynamics[Double]]],
                                binding: DynamicsSeq2Binding) = new Disposable { def dispose() {} }

  protected def startSeriesSeq3(run: RunBinding, name: String, data: Seq[Seq[Seq[Dynamics[Double]]]],
                                binding: DynamicsSeq3Binding) = new Disposable { def dispose() {} }

  protected def startSeriesSeq4(run: RunBinding, name: String, data: Seq[Seq[Seq[Seq[Dynamics[Double]]]]],
                                binding: DynamicsSeq4Binding) = new Disposable { def dispose() {} }

  protected def startSeriesSeq5(run: RunBinding, name: String, data: Seq[Seq[Seq[Seq[Seq[Dynamics[Double]]]]]],
                                binding: DynamicsSeq5Binding) = new Disposable { def dispose() {} }

  private def processRunStarted(run: RunBinding): Disposable = {

    experiment.simulation.runStartedInRun(run.index) subscribe ((x: Run) => {

      for (x <- run.names) {
        run.data += new XYSeries(x, false)
      }
    })
  }

  private def processRunFinished(run: RunBinding): Disposable = {

    experiment.simulation.runFinishedInRun(run.index) subscribe ((x: Run) => {

      run.file = generateFile(run)

      def save(c: JFreeChart) {

        val w = item.value.width
        val h = item.value.height

        val e = item.value.file match {

          case WritableFile(_, e) => e
          case UniqueFile(_, e) => e
        }

        e.toLowerCase match {

          case ".jpeg" => ChartUtilities.saveChartAsJPEG(run.file, c, w, h)
          case ".jpg" => ChartUtilities.saveChartAsJPEG(run.file, c, w, h)
          case ".png" => ChartUtilities.saveChartAsPNG(run.file, c, w, h)

          case x =>
            throw new IllegalArgumentException("The chart can be saved only as JPEG or PNG: " + x)
        }

        if (experiment.verbose) {
          println("Generated file " + run.file.getPath())
        }
      }

      try {

        val yname0 = item.value.yAxis match {

          case SingleAxis(yname0) => yname0
          case MultipleAxis if (run.names.length > 0) => run.names(0)
          case MultipleAxis => ""
        }

        val xy0 = new XYSeriesCollection {
          if (run.data.length > 0) addSeries(run.data(0))
        }

        val c = ChartFactory.createXYLineChart(item.value.title,
          item.value.x.name, yname0, xy0, PlotOrientation.VERTICAL,
          item.value.legend, item.value.tooltips, false)

        val p = c.getXYPlot()
        
        if (run.names.length > 0) {

          item.value.renderer match {

            case StandardXYRenderer =>
            case StepXYRenderer => p.setRenderer(0, new XYStepRenderer)
            case AreaXYRenderer => p.setRenderer(0, new XYAreaRenderer)
            case DotXYRenderer(w, h) => p.setRenderer(0, new XYDotRenderer {
              
              setDotWidth(w)
              setDotHeight(h)
            })
          }
        }

        for (i <- 1 to run.names.length - 1) {

          item.value.yAxis match {

            case SingleAxis(_) =>
              p.setDataset(i, new XYSeriesCollection { addSeries(run.data(i)) })

            case MultipleAxis =>

              val a = new NumberAxis(run.names(i))

              p.setRangeAxis(i, a)
              p.setRangeAxisLocation(i, AxisLocation.BOTTOM_OR_RIGHT)

              p.setDataset(i, new XYSeriesCollection { addSeries(run.data(i)) })
              p.mapDatasetToRangeAxis(i, i)
          }

          item.value.renderer match {

            case StandardXYRenderer => p.setRenderer(i, new StandardXYItemRenderer)
            case StepXYRenderer => p.setRenderer(i, new XYStepRenderer)
            case AreaXYRenderer => p.setRenderer(i, new XYAreaRenderer)
            case DotXYRenderer(w, h) => p.setRenderer(i, new XYDotRenderer {
              
              setDotWidth(w)
              setDotHeight(h)
            })
          }
        }

        save(c)

      } finally {
        run.data.clear()
      }
    })
  }

  private def processIntegPoint(run: RunBinding): Disposable = {

    val f = item.value.filter

    experiment.simulation.afterIntegPointInRun(run.index) subscribe ((p: Point) => {

      if (f.applyForBoolean(p)) {

        val x = item.value.x.data.applyForDouble(p)

        val vs = run.vars

        for (i <- 0 to run.vars.length - 1) {
          run.data(i).add(x, vs(i).applyForDouble(p))
        }
      }
    })
  }

  def toTOC: Seq[Node] = {

    val title = item.value.title
    <h4><a href={"#id" + id}>{title}</a></h4>
  }

  protected def toHtml(runs: Buffer[RunBinding]): Seq[Node] = {

    def uri(run: RunBinding): String = parent.uri(run.file)

    val title = item.value.title

    var b = Buffer[Node]()

    b += <h3 id={"id" + id}>{title}</h3>

    if (! item.value.description.isEmpty) {
      b += <p>{item.value.description}</p>
    }

    if (runs.length == 1) {
      b += <p><img src={uri(runs(0))} /></p>

    } else {

      def runTitle(i: Int): String = {

        item.value.runTitle
          .replace("$TITLE", item.value.title)
          .replace("$RUN_INDEX", (i + 1).toString)
          .replace("$RUN_COUNT", experiment.runCount.toString)
      }

      for (run <- runs) {

        b += <h4>{runTitle(run.index)}</h4>
        b += <p><img src={uri(run)} /></p>
      }
    }

    b
  }

  private def generateFile(run: RunBinding): File = {

    val replacement =
      Map("$TITLE" -> item.value.title,
        "$RUN_INDEX" -> (run.index + 1).toString,
        "$RUN_COUNT" -> experiment.runCount.toString)

    item.value.file.toFile(parent.file, replacement)
  }
}