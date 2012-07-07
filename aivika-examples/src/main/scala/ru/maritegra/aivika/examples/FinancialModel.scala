/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

/*
 * Vensim 5 Modeling Guide, Chapter Financial Modeling and Risk
 */

package ru.maritegra.aivika.examples

import ru.maritegra.aivika._
import ru.maritegra.aivika.SystemDynamics._

class FinancialModel {

  lazy val bookValue: Dynamics[Double] =
    integ(newInvestment - taxDepreciation, 0)

  lazy val taxDepreciation: Dynamics[Double] =
    bookValue / taxDepreciationTime

  lazy val taxableIncome: Dynamics[Double] =
    grossIncome - directCosts - losses -
      interestPayments - taxDepreciation

  lazy val production: Dynamics[Double] =
    availableCapacity

  lazy val availableCapacity: Dynamics[Double] =
    Dynamics.fromFunction(p =>
      if (time(p) >= buildingTime(p)) productionCapacity(p)
      else 0)

  val taxDepreciationTime: Dynamics[Double] = 10

  val taxRate: Dynamics[Double] = 0.4

  lazy val accountsReceivable: Dynamics[Double] =
    integ(billings - cashReceipts - losses,
      billings / (1 / averagePayableDelay + fractionalLossRate))

  val averagePayableDelay: Dynamics[Double] = 0.09

  lazy val awaitingBilling: Dynamics[Double] =
    integ(price * production - billings,
      price * production * billingProcessingTime)

  val billingProcessingTime: Dynamics[Double] = 0.04

  lazy val billings: Dynamics[Double] =
    awaitingBilling / billingProcessingTime
  
  lazy val borrowing: Dynamics[Double] =
    newInvestment * debtFinancingFraction

  val buildingTime: Dynamics[Double] = 1

  lazy val cashReceipts: Dynamics[Double] =
    accountsReceivable / averagePayableDelay

  lazy val debt: Dynamics[Double] =
    integ(borrowing - principalRepayment, 0)

  val debtFinancingFraction: Dynamics[Double] = 0.6

  val debtRetirementTime: Dynamics[Double] = 3

  lazy val directCosts: Dynamics[Double] =
    production * variableProductionCost

  val discountRate: Dynamics[Double] = 0.12

  val fractionalLossRate: Dynamics[Double] = 0.06

  lazy val grossIncome: Dynamics[Double] =
    billings

  lazy val interestPayments: Dynamics[Double] =
    debt * interestRate

  val interestRate: Dynamics[Double] = 0.12

  lazy val losses: Dynamics[Double] =
    accountsReceivable * fractionalLossRate

  lazy val netCashFlow: Dynamics[Double] =
    cashReceipts + borrowing - newInvestment -
      directCosts - interestPayments - principalRepayment - taxes

  lazy val netIncome: Dynamics[Double] =
    taxableIncome - taxes

  lazy val newInvestment: Dynamics[Double] =
    Dynamics.fromFunction(p =>
      if (time(p) >= buildingTime(p)) 0
      else (requiredInvestment(p) / buildingTime(p)))

  lazy val npvCashFlow: Dynamics[Double] =
    npv(netCashFlow, discountRate, 0, 1)
  
  lazy val npvIncome: Dynamics[Double] =
    npv(netIncome, discountRate, 0, 1)

  val price: Dynamics[Double] = 1

  lazy val principalRepayment: Dynamics[Double] =
    debt / debtRetirementTime

  val productionCapacity: Dynamics[Double] = 2400

  val requiredInvestment: Dynamics[Double] = 2000

  lazy val taxes: Dynamics[Double] =
    taxableIncome * taxRate

  val variableProductionCost: Dynamics[Double] = 0.6
}

object FinancialModel extends FinancialModel
