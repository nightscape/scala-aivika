/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

import scala.annotation.tailrec

class AgentState private (val agent: Agent, val parent: Option[AgentState]) {

  def this(agent: Agent) = this(agent, None)

  def this(parent: AgentState) = this(parent.agent, Some(parent))
  
  private class State(var version: Int, var active: Boolean)

  private val state = new RunMemo((run: Run) => new State(0, false))
  
  lazy val path: List[AgentState] = AgentState.path(this)
  
  def findPath(that: AgentState): (List[AgentState], List[AgentState]) = {
    
    if (this.agent == that.agent) {
      AgentState.partitionPath(this.path, that.path)
      
    } else {
      throw new IllegalStateException("Different agents.")
    }
  }

  protected val activate: Dynamics[Unit] = Dynamics.zero
  
  protected val deactivate: Dynamics[Unit] = Dynamics.zero

  private[aivika] def invokeActivate(p: Point) {

    val s = state(p.run)

    if (s.active) {
      throw new IllegalStateException("The agent state is already active")

    } else {

      s.version += 1
      s.active = true

      activate(p)
    }
  }

  private[aivika] def invokeDeactivate(p: Point) {

    val s = state(p.run)

    if (! s.active) {
      throw new IllegalStateException("The agent state is already inactive")

    } else {

      s.version += 1
      s.active = false

      deactivate(p)
    }
  }
  
  def addTimeout(dt: Double, action: Dynamics[Unit]): Dynamics[Unit] = new Dynamics[Unit] {
    def apply(p: Point): Unit = addTimeout(dt, action, p)
  }

  def addTimer(dt: Dynamics[Double], action: Dynamics[Unit]): Dynamics[Unit] = new Dynamics[Unit] {
    def apply(p: Point): Unit = addTimer(dt, action, p)
  }
  
  def addTimeout(dt: Double, action: Dynamics[Unit], p: Point) {

    val s = state(p.run)
    val v = s.version

    if (! s.active) {
      throw new IllegalStateException("The agent state must be active")

    } else {
    
      val m = new Dynamics[Unit] {
        
        def apply(p: Point): Unit = {
          
          if (s.version == v) {
            action(p)
          }
        }
      }

      val q = agent.queue
      q.enqueue(p.time + dt, m, p)
    }
  }
  
  def addTimer(dt: Dynamics[Double], action: Dynamics[Unit], p: Point) {
    
    val s = state(p.run)
    val v = s.version
    
    if (! s.active) {
      throw new IllegalStateException("The agent state must be active")

    } else {
    
      lazy val m1: Dynamics[Unit] = new Dynamics[Unit] {
        
        def apply(p: Point): Unit = {
          
          if (s.version == v) {
            
            m2(p)
            action(p)
          }
        }
      }
      
      lazy val m2: Dynamics[Unit] = new Dynamics[Unit] {
        
        def apply(p: Point): Unit = {
          
          val q = agent.queue
          q.enqueue(p.time + dt.applyForDouble(p), m1, p)
        }
      }

      m2(p)
    }
  }
}

private object AgentState {
  
  private def path(state: AgentState): List[AgentState] = path(state, List())

  @tailrec
  private def path(state: AgentState, acc: List[AgentState]): List[AgentState] = {

    state.parent match {

      case None => state :: acc
      case Some(parent) => path(parent, state :: acc)
    }
  }

  @tailrec
  private def partitionPath(path1: List[AgentState], path2: List[AgentState]): (List[AgentState], List[AgentState]) = {

    (path1, path2) match {

      case (h1 :: t1, List(h2)) if (h1 == h2) => (path1.reverse, path2)
      case (h1 :: t1, h2 :: t2) if (h1 == h2) => partitionPath(t1, t2)
      case _ => (path1.reverse, path2)
    }
  }
}
