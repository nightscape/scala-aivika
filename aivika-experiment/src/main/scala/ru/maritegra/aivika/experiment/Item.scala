/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment

sealed protected abstract class Item

protected case class TextItem(value: String) extends Item
protected case class HtmlItem(value: Html) extends Item

protected case class TimeSeriesItem(value: TimeSeries) extends Item
protected case class TableItem(value: Table) extends Item
protected case class LastValuesItem(value: LastValues) extends Item
protected case class HistogramItem(value: Histogram) extends Item
protected case class XYChartItem(value: XYChart) extends Item
protected case class StatisticsItem(value: Statistics) extends Item
protected case class DistributionChartItem(value: DistributionChart) extends Item
protected case class PieChartItem(value: PieChart) extends Item

protected case class CumulativeTableItem(value: CumulativeTable) extends Item
protected case class CumulativeXYChartItem(value: CumulativeXYChart) extends Item
protected case class CumulativeHistogramItem(value: CumulativeHistogram) extends Item
protected case class CumulativeStatisticsItem(value: CumulativeStatistics) extends Item
protected case class DeviationChartItem(value: DeviationChart) extends Item
protected case class CumulativeDistributionChartItem(value: CumulativeDistributionChart) extends Item
