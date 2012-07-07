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

private class LastValuesGenerator(parent: ExperimentGenerator, item: LastValuesItem, id: Int)
  extends ItemGenerator(parent, item) with BindableSeries[Any] {

  class Info(val name: String, var value: Option[Any])

  type RunRep = (Int, Buffer[Info])
  type DynamicsRep = Info
  type DynamicsSeqRep = Seq[Info]
  type DynamicsSeq2Rep = Seq[Seq[Info]]
  type DynamicsSeq3Rep = Seq[Seq[Seq[Info]]]
  type DynamicsSeq4Rep = Seq[Seq[Seq[Seq[Info]]]]
  type DynamicsSeq5Rep = Seq[Seq[Seq[Seq[Seq[Info]]]]]

  val experiment = parent.experiment
  val series = item.value.series

  init()

  protected def initRun(runIndex: Int): (Int, Buffer[Info]) = {
    (runIndex, Buffer())
  }

  private def addInfo(run: RunRep, name: String): Info = {

    val x = new Info(name, None)
    run._2 += x

    x
  }

  protected def initSeries(run: RunRep, name: String, data: Dynamics[Any]): DynamicsRep = {
    addInfo(run, name)
  }

  protected def initSeriesSeq(run: RunRep, name: String, data: Seq[Dynamics[Any]]): DynamicsSeqRep = {

    for {
      i <- 0 to data.length - 1

    } yield addInfo(run, name + "(" + i + ")")
  }

  protected def initSeriesSeq2(run: RunRep, name: String, data: Seq[Seq[Dynamics[Any]]]): DynamicsSeq2Rep = {

    for {
      i1 <- 0 to data.length - 1

    } yield for {
      i2 <- 0 to data(i1).length - 1

    } yield addInfo(run, name + "(" + i1 + ")(" + i2 + ")")
  }

  protected def initSeriesSeq3(run: RunRep, name: String, data: Seq[Seq[Seq[Dynamics[Any]]]]): DynamicsSeq3Rep = {

    for {
      i1 <- 0 to data.length - 1

    } yield for {
      i2 <- 0 to data(i1).length - 1

    } yield for {
        i3 <- 0 to data(i1)(i2).length - 1

      } yield addInfo(run, name + "(" + i1 + ")(" + i2 + ")(" + i3 + ")")
  }

  protected def initSeriesSeq4(run: RunRep, name: String, data: Seq[Seq[Seq[Seq[Dynamics[Any]]]]]): DynamicsSeq4Rep = {

    for {
      i1 <- 0 to data.length - 1

    } yield for {
      i2 <- 0 to data(i1).length - 1

    } yield for {
        i3 <- 0 to data(i1)(i2).length - 1

      } yield for {
          i4 <- 0 to data(i1)(i2)(i3).length - 1

        } yield addInfo(run, name + "(" + i1 + ")(" + i2 + ")(" + i3 + ")(" + i4 + ")")
  }

  protected def initSeriesSeq5(run: RunRep, name: String, data: Seq[Seq[Seq[Seq[Seq[Dynamics[Any]]]]]]): DynamicsSeq5Rep = {

    for {
      i1 <- 0 to data.length - 1

    } yield for {
      i2 <- 0 to data(i1).length - 1

    } yield for {
        i3 <- 0 to data(i1)(i2).length - 1

      } yield for {
          i4 <- 0 to data(i1)(i2)(i3).length - 1

        } yield for {
            i5 <- 0 to data(i1)(i2)(i3)(i4).length - 1

          } yield addInfo(run, name + "(" + i1 + ")(" + i2 + ")(" + i3 + ")(" + i4 + ")(" + i5 + ")")
  }

  protected def startRun(run: RunRep): Disposable = new Disposable {

    def dispose() {}
  }

  protected def startSeries(run: RunRep, name: String, data: Dynamics[Any],
                            binding: DynamicsRep): Disposable = {

    val h1 = experiment.simulation.pausedInRun(run._1) subscribe ((p: Point) => {
      binding.value = Some(data(p))
    })

    val h2 = experiment.simulation.onLastPointInRun(run._1) subscribe ((p: Point) => {
      binding.value = Some(data(p))
    })

    new Disposable {

      def dispose() {

        h1.dispose()
        h2.dispose()
      }
    }
  }

  protected def startSeriesSeq(run: RunRep, name: String, data: Seq[Dynamics[Any]],
                               binding: DynamicsSeqRep): Disposable = {

    val hs = for {
      i <- 0 to data.length - 1
      val name2 = name + "(" + i + ")"
      val data2 = data(i)
      val binding2 = binding(i)
    } yield startSeries(run, name2, data2, binding2)

    new Disposable {

      def dispose() {

        for (h <- hs) {
          h.dispose()
        }
      }
    }
  }

  protected def startSeriesSeq2(run: RunRep, name: String, data: Seq[Seq[Dynamics[Any]]],
                                binding: DynamicsSeq2Rep): Disposable = {

    val hs = for {
      i <- 0 to data.length - 1
      val name2 = name + "(" + i + ")"
      val data2 = data(i)
      val binding2 = binding(i)
    } yield startSeriesSeq(run, name2, data2, binding2)

    new Disposable {

      def dispose() {

        for (h <- hs) {
          h.dispose()
        }
      }
    }
  }

  protected def startSeriesSeq3(run: RunRep, name: String, data: Seq[Seq[Seq[Dynamics[Any]]]],
                                binding: DynamicsSeq3Rep): Disposable = {

    val hs = for {
      i <- 0 to data.length - 1
      val name2 = name + "(" + i + ")"
      val data2 = data(i)
      val binding2 = binding(i)
    } yield startSeriesSeq2(run, name2, data2, binding2)

    new Disposable {

      def dispose() {

        for (h <- hs) {
          h.dispose()
        }
      }
    }
  }

  protected def startSeriesSeq4(run: RunRep, name: String, data: Seq[Seq[Seq[Seq[Dynamics[Any]]]]],
                                binding: DynamicsSeq4Rep): Disposable = {

    val hs = for {
      i <- 0 to data.length - 1
      val name2 = name + "(" + i + ")"
      val data2 = data(i)
      val binding2 = binding(i)
    } yield startSeriesSeq3(run, name2, data2, binding2)

    new Disposable {

      def dispose() {

        for (h <- hs) {
          h.dispose()
        }
      }
    }
  }

  protected def startSeriesSeq5(run: RunRep, name: String, data: Seq[Seq[Seq[Seq[Seq[Dynamics[Any]]]]]],
                                binding: DynamicsSeq5Rep): Disposable = {

    val hs = for {
      i <- 0 to data.length - 1
      val name2 = name + "(" + i + ")"
      val data2 = data(i)
      val binding2 = binding(i)
    } yield startSeriesSeq4(run, name2, data2, binding2)

    new Disposable {

      def dispose() {

        for (h <- hs) {
          h.dispose()
        }
      }
    }
  }

  def toTOC: Seq[Node] = {

    val title = item.value.title
    <h4><a href={"#id" + id}>{title}</a></h4>
  }

  protected def toHtml(runs: Buffer[RunRep]): Seq[Node] = {
    
    val f = item.value.format

    def encode0(x: Info): Node = x.value match {

      case None => <p>{x.name} = ...</p>
      case Some(v) if f.isDefinedAt(v) => <p>{x.name} = {f(v)}</p>
      case Some(v) => <p>{x.name} = {v.toString}</p>
    }

    def encode(xs: Buffer[Info]): Seq[Node] = {
      for (x <- xs) yield encode0(x)
    }

    val title = item.value.title

    var b = Buffer[Node]()

    b += <h3 id={"id" + id}>{title}</h3>

    if (! item.value.description.isEmpty) {
      b += <p>{item.value.description}</p>
    }

    if (runs.length == 1) {
      b ++= encode(runs(0)._2)

    } else {

      def runTitle(i: Int): String = {

        item.value.runTitle
          .replace("$TITLE", item.value.title)
          .replace("$RUN_INDEX", (i + 1).toString)
          .replace("$RUN_COUNT", experiment.runCount.toString)
      }


      for (run <- runs) {

        b += <h4>{runTitle(run._1)}</h4>
        b ++= encode(run._2)
      }

    }

    b
  }
}