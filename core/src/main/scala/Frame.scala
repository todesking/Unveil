package com.todesking.unveil

case class Frame(locals: Map[Int, FrameItem], stack: List[FrameItem], effect: Effect) {
  def local(n: Int): FrameItem =
    locals(n)

  def stackTop: FrameItem = stack.head

  def pretty: String = s"""Locals:
${locals.map { case (k, v) => s"${k} = ${v}" }.mkString("\n")}
Stack:
${stack.mkString("\n")}"""
}

