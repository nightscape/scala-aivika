/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment

sealed abstract class HistogramType

case object FrequencyHistogram extends HistogramType
case object RelativeFrequencyHistogram extends HistogramType
case object ScaleAreaTo1Histogram extends HistogramType

