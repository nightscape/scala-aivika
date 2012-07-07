/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment

import ru.maritegra.aivika._

sealed abstract class Series[A]

case class DynamicsSeries[A](name: String, data: Dynamics[A]) extends Series[A]
case class DynamicsSeqSeries[A](name: String, data: Seq[Dynamics[A]]) extends Series[A]
case class DynamicsSeq2Series[A](name: String, data: Seq[Seq[Dynamics[A]]]) extends Series[A]
case class DynamicsSeq3Series[A](name: String, data: Seq[Seq[Seq[Dynamics[A]]]]) extends Series[A]
case class DynamicsSeq4Series[A](name: String, data: Seq[Seq[Seq[Seq[Dynamics[A]]]]]) extends Series[A]
case class DynamicsSeq5Series[A](name: String, data: Seq[Seq[Seq[Seq[Seq[Dynamics[A]]]]]]) extends Series[A]

object Series {
  
  def apply[A](name: String, data: Dynamics[A]): DynamicsSeries[A] = DynamicsSeries(name, data)
  def apply[A](name: String, data: Array[Dynamics[A]]): Series[A] = DynamicsSeqSeries(name, data toSeq)
  def apply[A](name: String, data: Array[Array[Dynamics[A]]]): Series[A] = DynamicsSeq2Series(name, data map (_ toSeq) toSeq)
  def apply[A](name: String, data: Array[Array[Array[Dynamics[A]]]]): Series[A] = DynamicsSeq3Series(name, data map (_ map (_ toSeq) toSeq) toSeq)
  def apply[A](name: String, data: Array[Array[Array[Array[Dynamics[A]]]]]): Series[A] = DynamicsSeq4Series(name, data map (_ map (_ map (_ toSeq) toSeq) toSeq) toSeq)
  def apply[A](name: String, data: Array[Array[Array[Array[Array[Dynamics[A]]]]]]): Series[A] = DynamicsSeq5Series(name, data map (_ map (_ map (_ map (_ toSeq) toSeq) toSeq) toSeq) toSeq)
}
