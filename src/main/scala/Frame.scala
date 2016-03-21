package com.todesking.unveil

case class Frame(locals: Map[Int, FrameItem], stack: List[FrameItem], effect: Effect) {
  def local(n: Int): FrameItem =
    locals(n)

  def stackTop: FrameItem = stack.head
}

