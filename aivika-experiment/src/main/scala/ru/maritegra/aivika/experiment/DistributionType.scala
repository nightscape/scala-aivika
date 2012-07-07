/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika.experiment

sealed abstract class DistributionType

case object DiscreteDistribution extends DistributionType
case object ContinuousDistribution extends DistributionType
