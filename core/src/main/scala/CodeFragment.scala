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

  def pretty: String = bytecode.map {
    case bc: Bytecode.HasAJumpTarget =>
      s"L${bc.label.innerId} $bc # L${jumpTargets(bc.jumpTarget).innerId}"
    case bc =>
      s"L${bc.label.innerId} $bc"
  }.mkString("\n")

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
}
object CodeFragment {
  def bytecode(bcs: Bytecode*): CodeFragment =
    CodeFragment(bcs, Map.empty)
}

