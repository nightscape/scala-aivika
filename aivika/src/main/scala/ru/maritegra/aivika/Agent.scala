/*
 * Copyright (C) 2009-2012 David Sorokin <david.sorokin@gmail.com>.
 *
 * All rights reserved.
 *
 * Licensed under BSD3. See LICENSE for details.
 */

package ru.maritegra.aivika

import java.lang.IllegalStateException

class Agent(val queue: EventQueue) {

  private val pastDataMessage = "Cannot request or update the past data. " +
    "The Agent.state field is like Ref. In case of need you can " +
    "wrap it in the Var variable listening to all changes and updating " +
    "accordingly the corresponded variable."

  private sealed abstract class AgentMode

  private case object CreationMode extends AgentMode
  private case object InitialMode extends AgentMode
  private case object TransientMode extends AgentMode
  private case object ProcessingMode extends AgentMode

  private class InnerState(var mode: AgentMode, var state: Option[AgentState], var time: Double)

  private val innerState = new RunMemo((run: Run) => new InnerState(CreationMode, None, Double.MinValue))
  
  val state = new Dynamics[Option[AgentState]] with Stateful {
    
    def apply(p: Point): Option[AgentState] = {
      
      queue.run(p)
      
      val s = innerState(p.run)

      if (p.time < s.time) {
        throw new IllegalStateException(pastDataMessage)

      } else {
        
        s.time = p.time
        s.state
      }
    }
  }
  
  private def traversePath(source: AgentState, dest: AgentState, p: Point, s: InnerState) {

    if (source != dest) {
      
      if (p.time < s.time) {
        throw new IllegalStateException(pastDataMessage)

      } else {

        val (path1, path2) = source.findPath(dest)

        s.mode = TransientMode
        s.time = p.time

        for (x <- path1) {

          s.state = Some(x)

          x.invokeDeactivate(p)
        }

        for (x <- path2) {

          if (x == dest) {
            s.mode = InitialMode
          }

          s.state = Some(x)

          x.invokeActivate(p)

          s.state match {

            case Some(y) if (y == dest) =>

               s.mode = ProcessingMode
               state.changedSource.trigger(this, p)

            case _ =>
          }
        }

        state.changedSource.trigger(this, p)
      }
    }
  }
  
  def activateState(st: AgentState): Dynamics[Unit] = Dynamics.fromFunction(activateState(st, _))

  def initState(st: AgentState): Dynamics[Unit] = Dynamics.fromFunction(initState(st, _))

  def activateState(st: AgentState, p: Point) {

    if (st.agent != this) {
      throw new IllegalStateException("The agent is different.")
      
    } else {
      
      queue.run(p)
      
      val s = innerState(p.run)
      
      s.mode match {

        case CreationMode =>
          
          st.parent match {

            case Some(_) =>
              
              throw new IllegalStateException("At time of running the agent " +
                "for the first time an initial state must be top-level.")

            case None =>

              if (p.time < s.time) {
                throw new IllegalStateException(pastDataMessage)

              } else {

                s.mode = InitialMode
                s.state = Some(st)
                s.time = p.time

                st.invokeActivate(p)

                s.state match {

                  case Some(y) if (y == st) =>

                    s.mode = ProcessingMode
                    state.changedSource.trigger(this, p)

                  case _ =>
                }
              }
          }

        case InitialMode =>

          throw new IllegalStateException("Use the Agent.initState method " +
            "during the state activation.")

        case TransientMode =>

          throw new IllegalStateException("Use the Agent.initState method " +
            "during the state activation.")

        case ProcessingMode =>

          val Some(st0) = s.state
          traversePath(st0, st, p, s)
      }
    }
  }

  def initState(st: AgentState, p: Point) {

    if (st.agent != this) {
      throw new IllegalStateException("The agent is different.")

    } else {

      queue.run(p)

      val s = innerState(p.run)

      s.mode match {

        case CreationMode =>

          throw new IllegalStateException("At time of running the agent " +
            "for the first time use the Agent.activateState method.")

        case InitialMode =>

          val Some(st0) = s.state
          traversePath(st0, st, p, s)

        case TransientMode =>

        case ProcessingMode =>

          throw new IllegalStateException("Use the Agent.activateState method " +
            "instead of Agent.initState everywhere outside of the state activation.")
      }
    }
  }
}
