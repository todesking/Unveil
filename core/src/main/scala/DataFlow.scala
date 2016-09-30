package com.todesking.unveil

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable

import java.lang.reflect.{ Method => JMethod, Constructor => JConstructor }

// TODO: self: Option[Data.Reference] for static methods
class DataFlow(val body: MethodBody, val self: Data.Reference) {

  // TODO: detailed values from merge
  def possibleValues(l: Bytecode.Label, p: DataPort): Seq[Data] =
    Seq(dataValues(l -> p))

  def onlyValue(l: Bytecode.Label, p: DataPort): Option[Data.Known] = {
    val pvs = possibleValues(l, p)
    if (pvs.size == 1) Some(pvs.head).collect { case d: Data.Known => d }
    else None
  }

  def toSSA: DataFlow.SSA =
    DataFlow.SSA.parse(this)

  def dataSource(l: Bytecode.Label, p: DataPort): DataSource =
    dataSources(l -> p)

  private[this] lazy val useSitesMap: Map[(Bytecode.Label, DataPort.Out), Map[Bytecode.Label, Set[DataPort.In]]] =
    body.bytecode.flatMap {
      case (l, bc) =>
        bc.inputs
          .map { p => p -> dataSource(l, p) }
          .collect { case (p1, DataSource.Bytecode(l2, p2)) => (l2, p2) -> (l, p1) }
    }.groupBy(_._1).mapValues(_.map(_._2)).map {
      case (k, uses) => k -> uses.groupBy(_._1).mapValues(_.map(_._2).toSet)
    }.toMap

  def useSites(l: Bytecode.Label, p: DataPort.Out): Seq[(Bytecode.Label, Bytecode, Set[DataPort.In])] =
    useSitesMap.get(l -> p).getOrElse(Map.empty).map { case (l, ps) => (l, body.bytecodeFromLabel(l), ps) }.toSeq

  def constructor(l: Bytecode.Label, p: DataPort): Option[MethodRef] = ???

  def argNum(l: Bytecode.Label, p: DataPort): Option[Int] =
    dataSources(l -> p) match {
      case DataSource.Argument(n) => Some(n)
      case _ => None
    }

  // Some(true): data has single value that point the instance
  // Some(false): data is not point the instance
  // None: not sure
  def isInstance(l: Bytecode.Label, p: DataPort, i: Instance[_ <: AnyRef]): Option[Boolean] =
    if(!dataType(l, p).isAssignableFrom(TypeRef.Reference(i.thisRef))) Some(false)
    else onlyValue(l, p).map(_.isInstance(i))

  def mayInstance(l: Bytecode.Label, p: DataPort, i: Instance[_ <: AnyRef]): Boolean =
    isInstance(l, p, i) != Some(false)

  def isThis(l: Bytecode.Label, p: DataPort): Option[Boolean] =
    isInstance(l, p, self.instance)

  def mustThis(l: Bytecode.Label, p: DataPort): Boolean =
    mustInstance(l, p, self.instance)

  // TODO: rm mr,bc
  def mustInstance(l: Bytecode.Label, p: DataPort, i: Instance[_ <: AnyRef], mr: MethodRef, bc: Bytecode): Boolean =
    isInstance(l, p, i).fold {
      throw new BytecodeTransformException(self.classRef, mr, body, bc, "Ambigious rererence")
    }(identity)

  def mustInstance(l: Bytecode.Label, p: DataPort, i: Instance[_ <: AnyRef]): Boolean =
    isInstance(l, p, i).fold {
      throw new MethodBodyAnalyzeException(body, s"Ambigious rererence: $l.$p")
    }(identity)

  def usedFieldsOf(i: Instance[_ <: AnyRef]): Set[(ClassRef, FieldRef)] =
    body.bytecode.foldLeft(Set.empty[(ClassRef, FieldRef)]) {
      case (agg, (label, bc)) =>
        import Bytecode._
        bc match {
          case bc: InstanceFieldAccess if mustInstance(label, bc.objectref, i) =>
            agg + (i.resolveField(bc.classRef, bc.fieldRef) -> bc.fieldRef)
          case _ => agg
        }
    }

  // TODO: DirectUsedMethod
  def usedMethodsOf(i: Instance[_ <: AnyRef]): Set[(ClassRef, MethodRef)] =
    body.bytecode.foldLeft(Set.empty[(ClassRef, MethodRef)]) {
      case (agg, (label, bc)) =>
        import Bytecode._
        bc match {
          case bc @ invokevirtual(cr, mr) if mayInstance(label, bc.objectref, i) =>
            agg + (i.resolveVirtualMethod(mr) -> mr)
          case bc @ invokeinterface(cr, mr, _) if mayInstance(label, bc.objectref, i) =>
            agg + (i.resolveVirtualMethod(mr) -> mr)
          case bc @ invokespecial(cr, mr) if mayInstance(label, bc.objectref, i) =>
            // TODO: Special method resolution
            agg + (cr -> mr)
          case _ => agg
        }
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
      case bc: Bytecode.Exit => Set(l)
      case bc => jumpDestinations(l).flatMap { l2 => possibleExits0(l2, ignore + l) }
    }

  lazy val jumpOrigins: Map[Bytecode.Label, Set[Bytecode.Label]] = {
    val m = Collections.newMultiMap[Bytecode.Label, Bytecode.Label]
    fallThroughs.foreach { case (from, to) => m.addBinding(to, from) }
    body.bytecode.foreach {
      case (label, bc: Bytecode.HasJumpTargets) =>
        bc.jumpTargets.foreach { t => m.addBinding(label, body.jumpTargets(label -> t)) }
      case _ =>
    }
    m.mapValues(_.toSet).toMap
  }

  def jumpDestinations(l: Bytecode.Label): Set[Bytecode.Label] =
    fallThroughs.get(l).toSet ++
      (body.labelToBytecode(l) match {
        case bc: Bytecode.HasJumpTargets => bc.jumpTargets.map { jt => body.jumpTargets(l -> jt) }
        case _ => Set.empty
      })

  lazy val initialFrame: Frame = {
    val thisData = if (body.isStatic) None else Some(FrameItem(DataSource.This, self))
    val argData = body.descriptor.args.zipWithIndex.flatMap {
      case (t, i) =>
        val source = DataSource.Argument(i)
        val data = Data.Unsure(t)
        if (t.isDoubleWord)
          Seq(
            FrameItem(source, data),
            FrameItem(source, data.secondWordData)
          )
        else
          Seq(FrameItem(source, data))
    }
    Frame((thisData.toSeq ++ argData).zipWithIndex.map(_.swap).toMap, List.empty)
  }
  def dataValue(l: Bytecode.Label, p: DataPort): Data =
    dataValues(l -> p)

  def dataType(l: Bytecode.Label, p: DataPort): TypeRef = dataValue(l, p).typeRef

  lazy val fallThroughs: Map[Bytecode.Label, Bytecode.Label] = {
    import Bytecode._
    body.bytecode.sliding(2).map {
      case Seq() => Map.empty
      case Seq(_) => Map.empty
      case Seq((l1, bc1: FallThrough), (l2, bc2)) => Map(l1 -> l2)
      case Seq(_, _) => Map.empty
    }.foldLeft(Map.empty[Bytecode.Label, Bytecode.Label]) { (a, m) => a ++ m }
  }

  // Yes I know this is just a pattern matching, not type-annotation. But I need readability
  lazy val (
    dataValues: Map[(Bytecode.Label, DataPort), Data],
    maxLocals: Int,
    maxStackDepth: Int,
    beforeFrames: Map[Bytecode.Label, Frame],
    dataSources: Map[(Bytecode.Label, DataPort), DataSource]
    ) = {
    val effectMerges = new AbstractLabel.Merger[Effect](Effect.fresh())
    def mergeData(d1: FrameItem, d2: FrameItem): FrameItem =
      d1.merge(d2)
    def merge(f1: Frame, f2: Frame): Frame = {
      Frame(
        (f1.locals.keySet ++ f2.locals.keySet)
        .filter { k => f1.locals.contains(k) && f2.locals.contains(k) }
        .map { k => (k -> mergeData(f1.locals(k), f2.locals(k))) }.toMap,
        f1.stack.zip(f2.stack).map { case (a, b) => mergeData(a, b) }
      )
    }

    val preFrames = mutable.HashMap.empty[Bytecode.Label, Frame]
    val updates = mutable.HashMap.empty[Bytecode.Label, FrameUpdate]
    val falls = mutable.HashMap.empty[Bytecode.Label, Bytecode.Label]

    val liveBcs = mutable.HashMap.empty[Bytecode.Label, Bytecode]

    val tasks = mutable.Set.empty[(Bytecode.Label, Frame)]
    tasks += (body.bytecode.head._1 -> initialFrame)

    while (tasks.nonEmpty) {
      val (pos, frame) = tasks.head
      tasks.remove(pos -> frame)
      val merged = preFrames.get(pos).map(merge(_, frame)) getOrElse frame
      if (preFrames.get(pos).map(_ != merged) getOrElse true) {
        preFrames(pos) = merged
        val bseq = body.bytecode.dropWhile(_._1 != pos)
        val (label, bc) = bseq.head
        assert(label == pos)
        liveBcs(label) = bc
        val u = try {
          bc.nextFrame(label, merged)
        } catch {
          case scala.util.control.NonFatal(e) =>
            throw new RuntimeException(s"Errow while dataflow analysis: ${e.getMessage}: ${label.format("L%d")} $bc, frame={\n${frame.pretty}\n}", e)
        }
        updates(label) = u
        bc match {
          case r: Bytecode.Return =>
          case j: Bytecode.Jump =>
            tasks += (body.jumpTargets(label -> j.target) -> u.newFrame)
          case b: Bytecode.Branch =>
            tasks += (body.jumpTargets(label -> b.target) -> u.newFrame)
            tasks += (bseq(1)._1 -> u.newFrame)
          case _: Bytecode.Procedure | _: Bytecode.Shuffle =>
            tasks += (bseq(1)._1 -> u.newFrame)
          case Bytecode.athrow() =>
          // TODO: Exception handler
        }
      }
    }


    val allFrames = preFrames.values ++ updates.values.map(_.newFrame)
    val maxLocals = allFrames.flatMap(_.locals.keys).max + 1
    val maxStackDepth = allFrames.map(_.stack.size).max
    val dataValues = updates.values.flatMap(_.dataValues).toMap
    val dataSources = updates.values.flatMap(_.dataSources).toMap

    (dataValues, maxLocals, maxStackDepth, preFrames.toMap, dataSources)
  }

  def pretty: String = {
    val format = "L%03d"
    def formatData(l: Bytecode.Label, p: DataPort, d: Data): String = {
      val typeStr = if (d.typeRef == self.typeRef) "this.class" else d.typeRef.toString
      val data = s"$typeStr = ${d.valueString}"
      isThis(l, p).fold {
        s"$data(this?)"
      } { yes =>
        if (yes) s"this"
        else data
      }
    }
    body.bytecode.map {
      case (label, bc) =>
        val base = s"${label.format(format)} ${bc.pretty}"
        val in = bc.inputs.map { in => s"  # ${in.name}: ${possibleValues(label, in).map(formatData(label, in, _)).mkString(", ")}" }
        val out = bc.output.map { out => s"  # ${out.name}: ${possibleValues(label, out).map(formatData(label, out, _)).mkString(", ")}" }.toSeq
        (Seq(base) ++ in ++ out).mkString("\n")
    }.mkString("\n")
  }
}

object DataFlow {
  sealed abstract trait Rewrite
  class SSA(
    instructions: Seq[SSA.Instruction],
    jumps: Map[(SSA.Label, JumpTarget), SSA.Label]
  ) {
    import SSA._

    def newInstances: Map[ValueLabel, Data.Initialized] = ???
    def useSites(v: ValueLabel): Map[Label, Instruction] = ???
    def rewrite(f: PartialFunction[(Label, Instruction), SSA]): SSA = ???
    def mustThis(v: ValueLabel): Boolean = ??? // throw if undecidable
    def escaped(v: ValueLabel): Boolean = ???
    def instance: Instance[_ <: AnyRef] = ???
    def bindArgs(args: Seq[ValueLabel]): SSA = ???
    def methodBody: MethodBody = ???
  }
  object SSA {
    def parse(df: DataFlow): SSA = ???
    case class ValueLabel private(globalUniqueId: Long)
    object ValueLabel {
      private[this] var _currentId: Long = 0L
      def fresh(): ValueLabel = synchronized {
        _currentId += 1
        ValueLabel(_currentId)
      }
    }
    case class Label(index: Int)

    sealed abstract class Instruction
    object Instruction {
      case class Phy(out: ValueLabel, ins: Seq[ValueLabel]) extends Instruction
      case class Procedure(out: Option[ValueLabel], ins: Seq[ValueLabel], bytecode: Bytecode.Procedure) extends Instruction
      case class Return(in: Option[ValueLabel]) extends Instruction
      case class Goto(jump: JumpTarget) extends Instruction
      case class Branch(jump: JumpTarget, in: ValueLabel, bytecode: Bytecode.Branch) extends Instruction
    }
    def bind(out: ValueLabel, value: ValueLabel): SSA =
      new SSA(Seq(Instruction.Phy(out, Seq(value))), Map())
  }
}
