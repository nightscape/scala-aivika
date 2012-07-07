/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment.generator

import scala.xml._

import ru.maritegra.aivika.util._
import ru.maritegra.aivika.experiment._

private abstract class ItemGenerator(parent: ExperimentGenerator, item: Item) {

  def run(): Disposable

  def toTOC: Seq[Node]

  def toHtml: Seq[Node]
}