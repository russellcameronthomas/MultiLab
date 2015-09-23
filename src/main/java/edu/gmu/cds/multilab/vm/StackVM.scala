/*
 * Copyright (C) 2015 russellthomas
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package edu.gmu.cds.multilab.vm

import akka.actor.FSM;



  /** A stack virtual machine with an instruction stack and an operand stack.
   *
   *Adapted from: http://lara.epfl.ch/w/cc09:vm_for_expressions
   *  
   */
  abstract class Actor {
    val name : String = ""
  }
  case class Agent() extends Actor {
    override val name: String = "an actor"
  }
  case class Automaton() extends Actor{
    override val name: String = "an automaton"
  }

  abstract class Instruction  
  case class Bipush(c : Int) extends Instruction  
  case class Iadd() extends Instruction
  case class Imul() extends Instruction
  case class Iload(slot : Int) extends Instruction
  case class Istore(slot : Int) extends Instruction
  
  class VM(var code    : Array[Instruction],
           var pc      : Int,        // program counter (current instruction)
           var local   : Array[Int], // local variables
           var operand : Array[Int], // operand stack
           var top     : Int,         // top of operand stack
           var myActor : Actor
  ) 
  {
    def step = {
      code(pc) match {
        case Bipush(c) => 
          operand(top + 1) = c
          top = top + 1
        case Iadd() =>
          operand(top - 1) = operand(top - 1) + operand(top)
          top = top - 1
        case Imul() => 
          operand(top - 1) = operand(top - 1) * operand(top)
          top = top - 1
        case Iload(n) => 
          operand(top + 1) = local(n)
          top = top + 1
        case Istore(n) => 
          local(n) = operand(top)
          top = top - 1
      }
      pc = pc + 1
    }
    def run = {
      while (pc < code.length)
        step
    }
  }
  
  class  StackVM {
    def run ()  = {
    val code : Array[Instruction] = List(
        Iload(0),
        Iload(1),
        Imul(),
        Iload(1),
        Iload(2),
        Imul(),
        Iadd(),
        Iload(0),
        Iload(2),
        Imul(),
        Iadd(),
        Bipush(2),
        Imul()).toArray
      val local : Array[Int] = List(3,4,5).toArray
      val operand = new Array[Int](10)
      val thisActor = new Agent()
      val vm = new VM(code,0,local,operand,0,thisActor);
      vm.run
      print("The Scala output from " + vm.myActor.name + " = ")
      println(vm.operand(vm.top))
    }
  }
  

  
  /*
  object StackVM  {
    def main(args : Array[String]): Unit =  {
      val code : Array[Instruction] = List(
        Iload(0),
        Iload(1),
        Imul(),
        Iload(1),
        Iload(2),
        Imul(),
        Iadd(),
        Iload(0),
        Iload(2),
        Imul(),
        Iadd(),
        Bipush(2),
        Imul()).toArray
      val local : Array[Int] = List(3,4,5).toArray
      val operand = new Array[Int](10)
      val thisActor = new Agent()
      val vm = new VM(code,0,local,operand,0,thisActor);
      vm.run
      println("The output from " + vm.myActor.name + " = ")
      println(vm.operand(vm.top))
    }
  }
 
  
  
*/