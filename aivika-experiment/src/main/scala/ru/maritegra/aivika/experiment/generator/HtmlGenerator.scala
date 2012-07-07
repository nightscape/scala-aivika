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

private class HtmlGenerator(parent: ExperimentGenerator, item: HtmlItem, id: Int)
  extends ItemGenerator(parent, item) {

  def run(): Disposable = new Disposable {
    def dispose() {}
  }

  def toTOC: Seq[Node] = List()

  def toHtml: Seq[Node] = {
    XML.loadString(item.value.code)
  }
}