/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment.generator

import scala.xml._
import scala.collection.mutable.Buffer

import ru.maritegra.aivika._
import ru.maritegra.aivika.util._
import ru.maritegra.aivika.experiment._
import ru.maritegra.aivika.statistics._

private class StatisticsGenerator(parent: ExperimentGenerator, item: StatisticsItem, id: Int)
  extends ItemGenerator(parent, item) with BindableSeries[Double] {

  val experiment = parent.experiment
  val series = item.value.series

  protected class RunBinding(val index: Int,
                             val vars: Buffer[Dynamics[Double]],
                             val names: Buffer[String],
                             val stats: Buffer[UpdatableTimeStatistics])

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
    new RunBinding(runIndex, Buffer(), Buffer(), Buffer())
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
    val vs = run.vars
    val ts = run.stats
    val is = Buffer[Int]()

    for (i <- 0 to vs.length - 1) {

      if (vs(i).isInstanceOf[Stateful]) {

        ts += new IrregularTimeStatistics
        hs += processStateful(run, i)

      } else {

        ts += new RegularTimeStatistics
        is += i
      }
      
    }

    hs += processIntegPoint(run, is)

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

  private def processIntegPoint(run: RunBinding, columns: Seq[Int]): Disposable = {

    val f = item.value.filter

    experiment.simulation.onIntegPointInRun(run.index) subscribe ((p: Point) => {

      if (f.applyForBoolean(p)) {

        val vs = run.vars
        val ts = run.stats
        
        for (i <- columns) {
          ts(i).add(p.time, vs(i).applyForDouble(p))
        }
      }
    })
  }

  private def processStateful(run: RunBinding, column: Int): Disposable = {

    val f = item.value.filter
    val v = run.vars(column)
    val t = run.stats(column)

    val stateful = v.asInstanceOf[Stateful]

    stateful.changedInRun(run.index) subscribe ((p: Point) => {

      if (f.applyForBoolean(p)) {
        t.add(p.time, v.applyForDouble(p))
      }
    })
  }

  def toTOC: Seq[Node] = {

    val title = item.value.title
    <h4><a href={"#id" + id}>{title}</a></h4>
  }

  protected def toHtml(runs: Buffer[RunBinding]): Seq[Node] = {

    val title = item.value.title

    var b = Buffer[Node]()

    b += <h3 id={"id" + id}>{title}</h3>

    if (! item.value.description.isEmpty) {
      b += <p>{item.value.description}</p>
    }

    if (runs.length == 1) {

      val run = runs(0)

      for (i <- 0 to run.names.length - 1) {

        b += <h4>{getName(run.names(i))}</h4>
        b ++= getInfo(run.stats(i))
      }

    } else {
      
      def contents(run: RunBinding): Seq[Node] = {

        var b = Buffer[Node]()
        
        for (i <- 0 to run.names.length - 1) {
          
          b += <h5>{getName(run.names(i))}</h5>
          b ++= getInfo(run.stats(i))
        }

        b
      }

      def runTitle(i: Int): String = {

        item.value.runTitle
          .replace("$TITLE", item.value.title)
          .replace("$RUN_INDEX", (i + 1).toString)
          .replace("$RUN_COUNT", experiment.runCount.toString)
      }

      for (run <- runs) {

        b += <h4>{runTitle(run.index)}</h4>
        b ++= contents(run)
      }
    }

    b
  }

  private def getName(x: String): String = {
    item.value.variableInfo.replace("$NAME", x)
  }

  private def getInfo(t: TimeStatistics): Seq[Node] = {

    val f = item.value.format
    
    def c(x: Double): String = {
      if (f.isDefinedAt(x)) f(x) else x.toString
    }

    val s1 = item.value.minimumInfo.replace("$VALUE", c(t.min)).replace("$TIME", c(t.timeOfMin))
    val s2 = item.value.maximumInfo.replace("$VALUE", c(t.max)).replace("$TIME", c(t.timeOfMax))

    val s3 = item.value.averageInfo.replace("$VALUE", c(t.mean))
    val s4 = item.value.deviationInfo.replace("$VALUE", c(math.sqrt(t.variance)))

    val b = Buffer[Node]()

    b += <p>{s1}</p>
    b += <p>{s2}</p>
    b += <p>{s3}</p>
    b += <p>{s4}</p>
    b
  }
}