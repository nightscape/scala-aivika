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

private class CumulativeTableGenerator(parent: ExperimentGenerator, item: CumulativeTableItem, id: Int)
  extends ItemGenerator(parent, item) with CumulativeBindableSeries[Any] {

  val experiment = parent.experiment
  val series = item.value.series

  private val lock = new Object

  protected class RunBinding(val vars: Buffer[Dynamics[Any]],
                             val names: Buffer[String],
                             val data: Buffer[(Int, Buffer[Any])],
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

  protected def initRun(): RunBinding = {
    new RunBinding(Buffer(), Buffer(), Buffer(), null)
  }

  protected def initSeries(run: RunBinding, name: String, data: Dynamics[Any]): DynamicsBinding = {

    run.vars += data
    run.names += name

    new DynamicsBinding()
  }

  protected def initSeriesSeq(run: RunBinding, name: String, data: Seq[Dynamics[Any]]): DynamicsSeqBinding = {

    for (i <- 0 to data.length - 1) {

      run.vars += data(i)
      run.names += (name + "(" + i + ")")
    }

    new DynamicsSeqBinding()
  }

  protected def initSeriesSeq2(run: RunBinding, name: String, data: Seq[Seq[Dynamics[Any]]]): DynamicsSeq2Binding = {

    for (i1 <- 0 to data.length - 1) {
      for (i2 <- 0 to data(i1).length - 1) {

        run.vars += data(i1)(i2)
        run.names += (name + "(" + i1 + ")(" + i2 + ")")
      }
    }

    new DynamicsSeq2Binding()
  }

  protected def initSeriesSeq3(run: RunBinding, name: String, data: Seq[Seq[Seq[Dynamics[Any]]]]): DynamicsSeq3Binding = {

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

  protected def initSeriesSeq4(run: RunBinding, name: String, data: Seq[Seq[Seq[Seq[Dynamics[Any]]]]]): DynamicsSeq4Binding = {

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

  protected def initSeriesSeq5(run: RunBinding, name: String, data: Seq[Seq[Seq[Seq[Seq[Dynamics[Any]]]]]]): DynamicsSeq5Binding = {

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

    hs += processLastPoint(run)
    hs += processSimulationFinished(run)

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

  private def processLastPoint(run: RunBinding): Disposable = {

    val f = item.value.filter

    experiment.simulation.afterLastPoint subscribe ((p: Point) => {

      if (f.applyForBoolean(p)) {

        val vs = run.vars
        val bs = vs map (_(p))

        val d = (p.run.index, bs)

        lock.synchronized {
          run.data += d
        }
      }

      ()
    })
  }

  private def processSimulationFinished(run: RunBinding): Disposable = {

    experiment.simulation.finished subscribe ((unit) => {

      run.file = generateFile()

      val b = run.data.sortBy(_._1)

      try {

        val w = new PrintWriter(new BufferedWriter(new FileWriter(run.file)))

        try {

          write(w, item.value.runColumn, run.names)

          for (x <- b) {
            write(w, x._1 + 1, x._2)
          }

        } finally {
          w.close()
        }

        if (experiment.verbose) {
          println("Generated file " + run.file.getPath())
        }

      } finally {
        run.data.clear()
      }
    })
  }

  private def write(w: PrintWriter, first: Any, data: Seq[Any]) {

    for (i <- 0 to data.length) {

      if (i > 0) {
        w.print(item.value.separator)
      }

      val x = if (i > 0) data(i - 1) else first

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

  protected def toHtml(run: RunBinding): Seq[Node] = {

    val uri = parent.uri(run.file)

    val title = item.value.title

    var b = Buffer[Node]()

    b += <h3 id={"id" + id}>{title}</h3>

    if (! item.value.description.isEmpty) {
      b += <p>{item.value.description}</p>
    }

    b += <p><a href={uri}>{item.value.link}</a></p>
    b
  }

  private def generateFile(): File = {

    val replacement =
      Map("$TITLE" -> item.value.title,
        "$RUN_COUNT" -> experiment.runCount.toString)

    item.value.file.toFile(parent.file, replacement)
  }
}