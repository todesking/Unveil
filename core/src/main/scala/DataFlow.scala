package com.todesking.unveil

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable

import java.lang.reflect.{ Method => JMethod, Constructor => JConstructor }

class DataFlow(val body: MethodBody, val self: Data.Reference) {
  def possibleValues(l: DataLabel): Seq[Data] = l match {
    case l: DataLabel.Out =>
      dataMerges.get(l) map { ms =>
        // TODO: Is this cause infinite loop?
        ms.toSeq.flatMap { m => possibleValues(m) }
      } getOrElse {
        Seq(dataValue(l))
      }
    case l: DataLabel.In =>
      possibleValues(dataBinding(l))
  }

  def onlyValue(l: DataLabel): Option[Data] = {
    val pvs = possibleValues(l)
    if (pvs.size == 1) Some(pvs.head)
    else None
  }

  // Some(true): data has single value that point the instance
  // Some(false): data is not point the instance
  // None: not sure
  def isInstance(l: DataLabel, i: Instance[_ <: AnyRef]): Option[Boolean] =
    onlyValue(l).map(_.isInstance(i)) orElse {
      if (possibleValues(l).exists(_.isInstance(i))) None
      else Some(false)
    }

  def isThis(l: DataLabel): Option[Boolean] =
    isInstance(l, self.instance)

  def mustThis(l: DataLabel): Boolean =
    mustInstance(l, self.instance)

  // TODO: refactor
  def mustInstance(l: DataLabel, i: Instance[_ <: AnyRef], mr: MethodRef, bc: Bytecode): Boolean =
    isInstance(l, i).fold {
      throw new BytecodeTransformException(self.classRef, mr, body, bc, "Ambigious rererence")
    }(identity)

  def mustInstance(l: DataLabel, i: Instance[_ <: AnyRef]): Boolean =
    isInstance(l, i).fold {
      throw new MethodBodyAnalyzeException(body, "Ambigious rererence")
    }(identity)

  def usedFieldsOf(i: Instance[_ <: AnyRef]): Set[(ClassRef, FieldRef)] =
    body.bytecode.foldLeft(Set.empty[(ClassRef, FieldRef)]) {
      case (agg, bc) =>
        import Bytecode._
        bc match {
          case bc: InstanceFieldAccess if mustInstance(bc.objectref, i) =>
            agg + (i.resolveField(bc.classRef, bc.fieldRef) -> bc.fieldRef)
          case _ => agg
        }
    }

  def usedMethodsOf(i: Instance[_ <: AnyRef]): Set[(ClassRef, MethodRef)] =
    body.bytecode.foldLeft(Set.empty[(ClassRef, MethodRef)]) {
      case (agg, bc) =>
        import Bytecode._
        bc match {
          case bc @ invokevirtual(cr, mr) if mustInstance(bc.objectref, i) =>
            agg + (i.resolveVirtualMethod(mr) -> mr)
          case bc @ invokeinterface(cr, mr, _) if mustInstance(bc.objectref, i) =>
            agg + (i.resolveVirtualMethod(mr) -> mr)
          case bc @ invokespecial(cr, mr) if mustInstance(bc.objectref, i) =>
            // TODO: Special method resolution
            agg + (cr -> mr)
          case _ => agg
        }
    }

  lazy val argLabels: Seq[DataLabel.Out] =
    body.descriptor.args
      .zipWithIndex
      .map { case (t, i) => DataLabel.out(s"arg_${i}") }

  def argNum(label: DataLabel.Out): Option[Int] = {
    val index = argLabels.indexOf(label)
    if (index == -1) None else Some(index)
  }

  def possibleReturns(l: Bytecode.Label): Seq[Bytecode.Return] =
    possibleExits(l).collect { case bc: Bytecode.Return => bc }

  def possibleExits(l: Bytecode.Label): Seq[Bytecode.Exit] =
    possibleExits0(l, Set.empty)
      .toSeq
      .map(body.labelToBytecode(_))
      .map(_.asInstanceOf[Bytecode.Exit])

  private[this] def possibleExits0(l: Bytecode.Label, ignore: Set[Bytecode.Label]): Set[Bytecode.Label] =
    if (ignore.contains(l)) Set.empty
    else body.labelToBytecode(l) match {
      case bc: Bytecode.Exit => Set(bc.label)
      case bc => jumpDestinations(l).flatMap { l2 => possibleExits0(l2, ignore + l) }
    }

  lazy val jumpOrigins: Map[Bytecode.Label, Set[Bytecode.Label]] = {
    val m = Collections.newMultiMap[Bytecode.Label, Bytecode.Label]
    fallThroughs.foreach { case (from, to) => m.addBinding(to, from) }
    body.bytecode.foreach {
      case bc: Bytecode.HasJumpTargets =>
        bc.jumpTargets.foreach { t => m.addBinding(bc.label, body.jumpTargets(t)) }
      case _ =>
    }
    m.mapValues(_.toSet).toMap
  }

  def jumpDestinations(l: Bytecode.Label): Set[Bytecode.Label] =
    fallThroughs.get(l).toSet ++
      (body.labelToBytecode(l) match {
        case bc: Bytecode.HasJumpTargets => bc.jumpTargets.map(body.jumpTargets)
        case _ => Set.empty
      })

  lazy val thisLabel: Option[DataLabel.Out] = if (body.isStatic) None else Some(DataLabel.out("this"))

  lazy val initialFrame: Frame = {
    val initialEffect = Effect.fresh()
    val thisData = thisLabel.map { l => FrameItem(l, self) }
    val argData = body.descriptor.args.zipWithIndex.zip(argLabels).flatMap {
      case ((t, i), label) =>
        val data = Data.Unsure(t)
        if (t.isDoubleWord)
          Seq(
            FrameItem(label, data),
            FrameItem(
              DataLabel.out(s"second word of ${label.name}"),
              data.secondWordData
            )
          )
        else
          Seq(FrameItem(label, data))
    }
    Frame((thisData.toSeq ++ argData).zipWithIndex.map(_.swap).toMap, List.empty, initialEffect)
  }
  def dataValue(l: DataLabel): Data =
    dataValues(l)

  def dataType(l: DataLabel): TypeRef = dataValues(l).typeRef

  def toDot(): String = {
    import Graphviz._
    val bcs = body.bytecode.map { bc => (bc.label -> bc) }.toMap
    val bcName = Bytecode.Label.namer("bytecode_", "")
    val dName = DataLabel.namer("data_", "")
    val eName = Effect.namer("effect_", "Eff#")
    s"""digraph {
graph[rankdir="BT"]
start[label="start" shape="doublecircle"]
${bcName.id(body.bytecode.head.label)} -> start
${eName.id(initialFrame.effect)} -> start [style="dotted"]
    ${
      body.bytecode.map { bc =>
        drawNode(bcName.id(bc.label), 'label -> bc.pretty, 'shape -> "rectangle")
      }.mkString("\n")
    }
    ${
      fallThroughs.map {
        case (src, d) =>
          drawEdge(bcName.id(d), bcName.id(src))
      }.mkString("\n")
    }
    ${
      body.bytecode.flatMap {
        case bc: Bytecode.Jump =>
          Seq(drawEdge(bcName.id(body.jumpTargets(bc.target)), bcName.id(bc.label)))
        case bc: Bytecode.Branch =>
          Seq(drawEdge(bcName.id(body.jumpTargets(bc.target)), bcName.id(bc.label), 'label -> "then"))
        case _ =>
          Seq.empty
      }.mkString("\n")
    }
    ${
      dataValues.collect {
        case (l: DataLabel.Out, data) =>
          drawNode(dName.id(l), 'label -> s"${l.name}: ${data}")
      }.mkString("\n")
    }
    ${
      body.bytecode.flatMap { bc =>
        bc.inputs.flatMap { i =>
          dataBinding.get(i).map(i -> _)
        }.map {
          case (i, o) =>
            drawEdge(bcName.id(bc.label), dName.id(o), 'style -> "dotted", 'label -> i.name)
        }
      }.mkString("\n")
    }
    ${
      body.bytecode.flatMap { bc => bc.output.map(bc -> _) }.map {
        case (bc, o) =>
          drawEdge(dName.id(o), bcName.id(bc.label), 'style -> "dotted", 'label -> o.name)
      }.mkString("\n")
    }
    ${
      dataMerges.flatMap {
        case (m, ds) =>
          ds.map { d => drawEdge(dName.id(m), dName.id(d), 'style -> "dotted") }
      }.mkString("\n")
    }
    ${
      effectMerges.flatMap {
        case (m, es) =>
          es.map { e => drawEdge(eName.id(m), eName.id(e), 'style -> "dotted") }
      }.mkString("\n")
    }
    ${
      effectDependencies.map {
        case (bcl, e) =>
          drawEdge(bcName.id(bcl), eName.id(e), 'style -> "dotted")
      }.mkString("\n")
    }
    ${
      body.bytecode.flatMap { bc => bc.effect.map(bc -> _) }.map {
        case (bc, e) =>
          drawEdge(eName.id(e), bcName.id(bc.label), 'style -> "dotted")
      }.mkString("\n")
    }
}"""
  }

  lazy val fallThroughs: Map[Bytecode.Label, Bytecode.Label] = {
    import Bytecode._
    body.bytecode.sliding(2).map {
      case Seq() => Map.empty
      case Seq(_) => Map.empty
      case Seq(bc1: FallThrough, bc2) => Map(bc1.label -> bc2.label)
      case Seq(_, _) => Map.empty
    }.foldLeft(Map.empty[Bytecode.Label, Bytecode.Label]) { (a, m) => a ++ m }
  }

  // Yes I know this is just a pattern matching, not type-annotation. But I need readability
  lazy val (
    dataBinding: Map[DataLabel.In, DataLabel.Out],
    dataValues: Map[DataLabel, Data],
    dataMerges: Map[DataLabel.Out, Set[DataLabel.Out]],
    effectDependencies: Map[Bytecode.Label, Effect],
    effectMerges: Map[Effect, Set[Effect]],
    liveBytecode: Seq[Bytecode],
    maxLocals: Int,
    maxStackDepth: Int,
    beforeFrames: Map[Bytecode.Label, Frame]
    ) = {
    val dataMerges = new AbstractLabel.Merger[DataLabel.Out](DataLabel.out("merged"))
    val effectMerges = new AbstractLabel.Merger[Effect](Effect.fresh())
    def mergeData(d1: FrameItem, d2: FrameItem): FrameItem =
      FrameItem(dataMerges.merge(d1.label, d2.label), Data.merge(d1.data, d2.data)) // TODO: record placedBy merge
    def merge(f1: Frame, f2: Frame): Frame = {
      Frame(
        (f1.locals.keySet ++ f2.locals.keySet)
        .filter { k => f1.locals.contains(k) && f2.locals.contains(k) }
        .map { k => (k -> mergeData(f1.locals(k), f2.locals(k))) }.toMap,
        f1.stack.zip(f2.stack).map { case (a, b) => mergeData(a, b) },
        effectMerges.merge(f1.effect, f2.effect)
      )
    }

    val preFrames = mutable.HashMap.empty[Bytecode.Label, Frame]
    val updates = mutable.HashMap.empty[Bytecode.Label, FrameUpdate]
    val falls = mutable.HashMap.empty[Bytecode.Label, Bytecode.Label]

    val liveBcs = mutable.HashMap.empty[Bytecode.Label, Bytecode]

    val tasks = mutable.Set.empty[(Bytecode.Label, Frame)]
    tasks += (body.bytecode.head.label -> initialFrame)

    while (tasks.nonEmpty) {
      val (pos, frame) = tasks.head
      tasks.remove(pos -> frame)
      val merged = preFrames.get(pos).map(merge(_, frame)) getOrElse frame
      if (preFrames.get(pos).map(_ != merged) getOrElse true) {
        preFrames(pos) = merged
        val bseq = body.bytecode.dropWhile(_.label != pos)
        val bc = bseq.head
        liveBcs(bc.label) = bc
        val u = bc.nextFrame(merged)
        updates(bc.label) = u
        bc match {
          case r: Bytecode.Return =>
          case j: Bytecode.Jump =>
            tasks += (body.jumpTargets(j.target) -> u.newFrame)
          case b: Bytecode.Branch =>
            tasks += (body.jumpTargets(b.target) -> u.newFrame)
            tasks += (bseq(1).label -> u.newFrame)
          case _: Bytecode.Procedure | _: Bytecode.Shuffle =>
            tasks += (bseq(1).label -> u.newFrame)
          case Bytecode.athrow() =>
          // TODO: Exception handler
        }
      }
    }

    val dataValues = mutable.HashMap.empty[DataLabel, Data]
    (preFrames.values.toSeq :+ initialFrame) foreach { frame =>
      (frame.locals.values ++ frame.stack) foreach { d =>
        dataValues(d.label) = d.data
      }
    }
    val binding = mutable.HashMap.empty[DataLabel.In, DataLabel.Out]
    val effectDependencies = mutable.HashMap.empty[Bytecode.Label, Effect]
    updates.values foreach { u =>
      dataValues ++= u.dataValues.mapValues(_.data)
      binding ++= u.binding
      effectDependencies ++= u.effectDependencies
    }

    val allFrames = preFrames.values ++ updates.values.map(_.newFrame)
    val maxLocals = allFrames.flatMap(_.locals.keys).max + 1
    val maxStackDepth = allFrames.map(_.stack.size).max

    (binding.toMap, dataValues.toMap, dataMerges.toMap, effectDependencies.toMap, effectMerges.toMap, liveBcs.values.toSeq, maxLocals, maxStackDepth, preFrames.toMap)
  }
}
