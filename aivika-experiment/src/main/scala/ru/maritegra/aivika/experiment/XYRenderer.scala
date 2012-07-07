/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment

sealed abstract class XYRenderer

case object StandardXYRenderer extends XYRenderer
case object StepXYRenderer extends XYRenderer
case object AreaXYRenderer extends XYRenderer
case class DotXYRenderer(width: Int,  height: Int) extends XYRenderer
