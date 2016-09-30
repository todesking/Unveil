package com.todesking.unveil

class CodeFragment(val bytecodeSeq: Seq[Bytecode], val jumpTargets: Map[(Bytecode.Label, JumpTarget), Bytecode.Label] = Map.empty) {
  {
    require(bytecodeSeq.nonEmpty)
    val jts = bytecode.collect { case (l, bc: Bytecode.HasJumpTargets) =>
      bc.jumpTargets.map(l -> _)
    }.flatten.toSet
    require(jts.size == jumpTargets.size)
    require(bytecode.forall {
      case (l, bc: Bytecode.HasJumpTargets) =>
        bc.jumpTargets.forall { jt => jumpTargets.contains(l -> jt) }
      case _ => true
    })
  }

  // memo: used from ctor
  lazy val bytecode: Seq[(Bytecode.Label, Bytecode)] =
    bytecodeSeq.zipWithIndex.map { case (b, i) => Bytecode.Label(i) -> b }

  def jumpDestination(bcl: Bytecode.Label, jt: JumpTarget): Bytecode.Label =
    jumpTargets(bcl -> jt)

  def bytecodeFromLabel(l: Bytecode.Label): Bytecode =
    labelToBytecode(l)

  def pretty: String =
    s"""${
      bytecode.map {
        case (l, bc) =>
          val format = "L%03d"
          l.format(format) + " " + (bc match {
            case bc: Bytecode.HasAJumpTarget =>
              s"${bc.pretty} # ${jumpDestination(l, bc.jumpTarget).format(format)}"
            case bc =>
              bc.pretty
          })
      }.mkString("\n")
    }
"""

  def methodReferences: Set[(ClassRef, MethodRef)] =
    bytecode.collect { case bc: Bytecode.HasMethodRef => (bc.classRef -> bc.methodRef) }.toSet

  def fieldReferences: Set[(ClassRef, FieldRef)] =
    bytecode.collect { case bc: Bytecode.HasFieldRef => (bc.classRef -> bc.fieldRef) }.toSet

  lazy val labelToBytecode: Map[Bytecode.Label, Bytecode] =
    bytecode.toMap

  def rewrite_**(f: PartialFunction[(Bytecode.Label, Bytecode), Map[Bytecode.Label, CodeFragment]]): CodeFragment = {
    val liftedF = f.lift
    val allRewrites =
      bytecode.foldLeft(Map.empty[Bytecode.Label, CodeFragment]) {
        case (m, lbc @ (l, bc)) =>
          liftedF(lbc).fold(m) { mm =>
            Algorithm.sharedNothingUnion(m, mm).fold {
              throw new TransformException(s"rewrite conflict")
            }(identity)
          }
      }
    rewrite0(allRewrites)
  }

  def prependBytecode(bcs: Seq[Bytecode]): CodeFragment =
    prepend(new CodeFragment(bcs))

  def prepend(cf: CodeFragment): CodeFragment = {
    val newJts = cf.jumpTargets ++ jumpTargets.map { case ((l, jt), dest) =>
      (l.offset(cf.bytecode.size) -> jt) -> dest.offset(cf.bytecode.size)
    }
    val newBcs = cf.bytecodeSeq ++ bytecodeSeq
    new CodeFragment(newBcs, newJts)
  }

  private[this] def rewrite0(rewrites: Map[Bytecode.Label, CodeFragment]): CodeFragment = {
    def adjustIndex(start: Int, shift: Int, l: Bytecode.Label) =
      if (l.index > start) l.offset(shift) else l
    val (bcs, jts, _) =
      rewrites.toSeq.sortBy(_._1.index).foldLeft((bytecodeSeq, jumpTargets, 0)) { case ((bcs, jts, offset), (label, cf)) =>
        val start = label.index + offset
        require(0 <= start && start < bcs.size)
        require(cf.bytecode.nonEmpty) // Dataflow could broken if just remove a bytecode
        val newBcs = bcs.patch(start, cf.bytecodeSeq, 1)
        val shift = cf.bytecode.size - 1
        val newJts = jts.map { case ((l, jt), dest) => (adjustIndex(start, shift, l) -> jt) -> adjustIndex(start, shift, dest) } ++
          cf.jumpTargets.map { case ((l, jt), dest) => (l.offset(start) -> jt) -> dest.offset(start) }
        (newBcs, newJts, offset + shift)
      }
    new CodeFragment(bcs, jts)
  }
}
object CodeFragment {
  def bytecode(bcs: Bytecode*): CodeFragment =
    new CodeFragment(bcs, Map.empty)
}

