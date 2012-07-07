/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment

import scala.collection.mutable.Buffer

class ItemBuffer {

  protected[experiment] val buffer = Buffer[Item]()

  def +=(item: String) {
    buffer += TextItem(item)
  }

  def +=(item: Html) {
    buffer += HtmlItem(item)
  }

  def +=(item: TimeSeries) {
    buffer += TimeSeriesItem(item)
  }

  def +=(item: Table) {
    buffer += TableItem(item)
  }

  def +=(item: LastValues) {
    buffer += LastValuesItem(item)
  }

  def +=(item: Histogram) {
    buffer += HistogramItem(item)
  }

  def +=(item: XYChart) {
    buffer += XYChartItem(item)
  }

  def +=(item: Statistics) {
    buffer += StatisticsItem(item)
  }

  def +=(item: DistributionChart) {
    buffer += DistributionChartItem(item)
  }

  def +=(item: PieChart) {
    buffer += PieChartItem(item)
  }

  def +=(item: CumulativeXYChart) {
    buffer += CumulativeXYChartItem(item)
  }

  def +=(item: CumulativeTable) {
    buffer += CumulativeTableItem(item)
  }

  def +=(item: CumulativeHistogram) {
    buffer += CumulativeHistogramItem(item)
  }

  def +=(item: CumulativeStatistics) {
    buffer += CumulativeStatisticsItem(item)
  }

  def +=(item: DeviationChart) {
    buffer += DeviationChartItem(item)
  }

  def +=(item: CumulativeDistributionChart) {
    buffer += CumulativeDistributionChartItem(item)
  }
}