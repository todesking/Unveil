package com.todesking.unveil

case class CodeFragment(bytecode: Seq[Bytecode], jumpTargets: Map[JumpTarget, Bytecode.Label] = Map.empty) {
  {
    val labels = bytecode.map(_.label).toSet
    val jts = bytecode.collect { case bc: Bytecode.HasJumpTargets => bc.jumpTargets }.flatten
    require(jumpTargets.values.forall { k => labels.contains(k) })
    require(jts.forall { jt => jumpTargets.contains(jt) })
  }

  def prependBytecode(bcs: Seq[Bytecode]): CodeFragment =
    copy(bytecode = bcs ++ bytecode)

  def pretty: String =
    s"""${
      bytecode.map { bc =>
        val l = f"L${bc.label.innerId}%-5s "
        l + (bc match {
          case bc: Bytecode.HasAJumpTarget =>
            s"${bc.pretty} # L${jumpTargets(bc.jumpTarget).innerId}"
          case bc =>
            bc.pretty
        })
      }.mkString("\n")
    }
"""

  def fresh(): CodeFragment = {
    val bcs = bytecode.map { bc => bc -> bc.fresh() }
    val oldToNew = bcs.map { case (old, new_) => old.label -> new_.label }.toMap
    val newBcMap = bcs.map { case (_, bc) => bc.label -> bc }.toMap
    val newJts = bcs.collect {
      // TODO: suppoort HasJumpTargets
      case (old: Bytecode.HasAJumpTarget, bc: Bytecode.HasAJumpTarget) =>
        (bc.jumpTarget -> oldToNew(jumpTargets(old.jumpTarget)))
    }.toMap
    CodeFragment(bcs.map(_._2), newJts)
  }

  def methodReferences: Set[(ClassRef, MethodRef)] =
    bytecode.collect { case bc: Bytecode.HasMethodRef => (bc.classRef -> bc.methodRef) }.toSet

  def fieldReferences: Set[(ClassRef, FieldRef)] =
    bytecode.collect { case bc: Bytecode.HasFieldRef => (bc.classRef -> bc.fieldRef) }.toSet

  lazy val labelToBytecode: Map[Bytecode.Label, Bytecode] =
    bytecode.map { bc => bc.label -> bc }.toMap

  def rewrite_**(f: PartialFunction[Bytecode, Map[Bytecode.Label, CodeFragment]]): CodeFragment = {
    val lifted = f.lift
    val allRewrites =
      bytecode.foldLeft(Map.empty[Bytecode.Label, CodeFragment]) {
        case (m, bc) =>
          lifted(bc).fold(m) { mm =>
            Algorithm.sharedNothingUnion(m, mm).fold {
              throw new TransformException(s"rewrite conflict")
            }(identity)
          }
      }
    allRewrites.foldLeft(this) { case (b, (l, bcs)) => b.replaceBytecode(l, bcs) }
  }

  def replaceBytecode(l: Bytecode.Label, newBc: Bytecode): CodeFragment =
    replaceBytecode(l, CodeFragment.bytecode(newBc))

  def replaceBytecode(l: Bytecode.Label, cf: CodeFragment): CodeFragment = {
    require(labelToBytecode.contains(l))
    require(cf.bytecode.nonEmpty)
    if (cf.bytecode.size == 1 && cf.bytecode.head.label == l) { // TODO: WHY????
      // cf.jumpTargets could safely ignored
      this
    } else {
      val newCf = cf.fresh()
      val bcs = newCf.bytecode
      val first = bcs.head
      val start = bytecode.indexWhere(_.label == l)
      assert(start >= 0)
      val newBcs = bytecode.patch(start, bcs, 1)
      val newJts = jumpTargets.map { case (jt, bcl) => if (bcl == l) (jt -> first.label) else (jt -> bcl) } ++ newCf.jumpTargets
      copy(bytecode = newBcs, jumpTargets = newJts)
    }
  }
}
object CodeFragment {
  def bytecode(bcs: Bytecode*): CodeFragment =
    CodeFragment(bcs, Map.empty)
}

