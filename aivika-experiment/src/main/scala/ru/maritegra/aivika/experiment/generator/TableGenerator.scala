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

import ru.maritegra.aivika._
import ru.maritegra.aivika.util._
import ru.maritegra.aivika.experiment._

private class TableGenerator(parent: ExperimentGenerator, item: TableItem, id: Int)
  extends ItemGenerator(parent, item) with BindableSeries[Any] {

  val experiment = parent.experiment
  val series = item.value.series

  protected class RunBinding(val index: Int,
                             val vars: Buffer[Dynamics[Any]],
                             val columns: Buffer[String],
                             var file: File,
                             var writer: PrintWriter)

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
    new RunBinding(runIndex, Buffer(), Buffer(), null, null)
  }

  protected def initSeries(run: RunBinding, name: String, data: Dynamics[Any]): DynamicsBinding = {

    run.vars += data
    run.columns += name

    new DynamicsBinding()
  }

  protected def initSeriesSeq(run: RunBinding, name: String, data: Seq[Dynamics[Any]]): DynamicsSeqBinding = {

    for (i <- 0 to data.length - 1) {

      run.vars += data(i)
      run.columns += (name + "(" + i + ")")
    }

    new DynamicsSeqBinding()
  }

  protected def initSeriesSeq2(run: RunBinding, name: String, data: Seq[Seq[Dynamics[Any]]]): DynamicsSeq2Binding = {

    for (i1 <- 0 to data.length - 1) {
      for (i2 <- 0 to data(i1).length - 1) {

        run.vars += data(i1)(i2)
        run.columns += (name + "(" + i1 + ")(" + i2 + ")")
      }
    }

    new DynamicsSeq2Binding()
  }

  protected def initSeriesSeq3(run: RunBinding, name: String, data: Seq[Seq[Seq[Dynamics[Any]]]]): DynamicsSeq3Binding = {

    for (i1 <- 0 to data.length - 1) {
      for (i2 <- 0 to data(i1).length - 1) {
        for (i3 <- 0 to data(i1)(i2).length - 1) {

          run.vars += data(i1)(i2)(i3)
          run.columns += (name + "(" + i1 + ")(" + i2 + ")(" + i3 + ")")
        }
      }
    }

    new DynamicsSeq3Binding()
  }

  protected def initSeriesSeq4(run: RunBinding, name: String, data: Seq[Seq[Seq[Seq[Dynamics[Any]]]]]): DynamicsSeq4Binding = {

    for (i1 <- 0 to data.length - 1) {
      for (i2 <- 0 to data(i1).length - 1) {
        for (i3 <- 0 to data(i1)(i2).length - 1) {
          for (i4 <- 0 to data(i1)(i2)(i3).length - 1) {

            run.vars += data(i1)(i2)(i3)(i4)
            run.columns += (name + "(" + i1 + ")(" + i2 + ")(" + i3 + ")(" + i4 + ")")
          }
        }
      }
    }

    new DynamicsSeq4Binding()
  }

  protected def initSeriesSeq5(run: RunBinding, name: String, data: Seq[Seq[Seq[Seq[Seq[Dynamics[Any]]]]]]): DynamicsSeq5Binding = {

    for (i1 <- 0 to data.length - 1) {
      for (i2 <- 0 to data(i1).length - 1) {
        for (i3 <- 0 to data(i1)(i2).length - 1) {
          for (i4 <- 0 to data(i1)(i2)(i3).length - 1) {
            for (i5 <- 0 to data(i1)(i2)(i3)(i4).length - 1) {

              run.vars += data(i1)(i2)(i3)(i4)(i5)
              run.columns += (name + "(" + i1 + ")(" + i2 + ")(" + i3 + ")(" + i4 + ")(" + i5 + ")")
            }
          }
        }
      }
    }

    new DynamicsSeq5Binding()
  }

  protected def startRun(run: RunBinding): Disposable = {

    val hs = Buffer[Disposable]()
    val vs = run.vars

    hs += processRunStarted(run)

    for (i <- 0 to vs.length - 1) {

      if (vs(i).isInstanceOf[Stateful]) {
        hs += processStateful(run, i)
      }
    }

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

  protected def startSeries(run: RunBinding, name: String, data: Dynamics[Any],
                            binding: DynamicsBinding) = new Disposable { def dispose() {} }

  protected def startSeriesSeq(run: RunBinding, name: String, data: Seq[Dynamics[Any]],
                               binding: DynamicsSeqBinding) = new Disposable { def dispose() {} }

  protected def startSeriesSeq2(run: RunBinding, name: String, data: Seq[Seq[Dynamics[Any]]],
                                binding: DynamicsSeq2Binding) = new Disposable { def dispose() {} }

  protected def startSeriesSeq3(run: RunBinding, name: String, data: Seq[Seq[Seq[Dynamics[Any]]]],
                                binding: DynamicsSeq3Binding) = new Disposable { def dispose() {} }

  protected def startSeriesSeq4(run: RunBinding, name: String, data: Seq[Seq[Seq[Seq[Dynamics[Any]]]]],
                                binding: DynamicsSeq4Binding) = new Disposable { def dispose() {} }

  protected def startSeriesSeq5(run: RunBinding, name: String, data: Seq[Seq[Seq[Seq[Seq[Dynamics[Any]]]]]],
                                binding: DynamicsSeq5Binding) = new Disposable { def dispose() {} }

  private def processRunStarted(run: RunBinding): Disposable = {

    experiment.simulation.runStartedInRun(run.index) subscribe ((x: Run) => {

      run.file = generateFile(run)
      run.writer = new PrintWriter(new BufferedWriter(new FileWriter(run.file)))

      write(run, run.columns)
    })
  }

  private def processRunFinished(run: RunBinding): Disposable = {

    experiment.simulation.runFinishedInRun(run.index) subscribe ((x: Run) => {

      run.writer.close()

      if (experiment.verbose) {
        println("Generated file " + run.file.getPath())
      }
    })
  }

  private def processIntegPoint(run: RunBinding): Disposable = {

    val f = item.value.filter

    experiment.simulation.afterIntegPointInRun(run.index) subscribe ((p: Point) => {

      if (f.apply(p)) {

        val vs = run.vars
        val xs = new Array[Any](vs.length)

        for (i <- 0 to vs.length - 1) {
          xs(i) = vs(i)(p)
        }

        write(run, xs)
      }
    })
  }

  private def processStateful(run: RunBinding, column: Int): Disposable = {

    val f = item.value.filter
    val v = run.vars(column)

    val stateful = v.asInstanceOf[Stateful]

    stateful.changedInRun(run.index) subscribe ((p: Point) => {

      if (f.apply(p)) {

        val x = v(p)

        val vs = run.vars
        val xs = new Array[Any](vs.length)

        for (i <- 0 to vs.length - 1) {
          xs(i) = if (i == column) x else vs(i)(p)
        }

        write(run, xs)
      }
    })
  }

  private def write(run: RunBinding, data: Seq[Any]) {

    val w = run.writer

    for (i <- 0 to data.length - 1) {

      if (i > 0) {
        w.print(item.value.separator)
      }
      
      val x = data(i)
      
      if (item.value.format.isDefinedAt(x)) {
        w.print(item.value.format(x))

      } else {
        w.print(x)
      }
    }

    w.println()
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
      b += <p><a href={uri(runs(0))}>{item.value.link}</a></p>

    } else {
      
      def runLink(i: Int): String = {

        item.value.runLink
          .replace("$TITLE", item.value.title)
          .replace("$LINK", item.value.link)
          .replace("$RUN_INDEX", (i + 1).toString)
          .replace("$RUN_COUNT", experiment.runCount.toString)
      }

      for (run <- runs) {
        b += <p><a href={uri(run)}>{runLink(run.index)}</a></p>
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