package com.todesking.unveil

abstract class CodeFragment {
  def bytecodeSeq: Seq[Bytecode]

  def size: Int

  def nonEmpty: Boolean = size > 0

  def incompleteJumpTargets: Map[(Bytecode.Label, JumpTarget), Either[String, Bytecode.Label]]

  def nameToLabel: Map[String, Bytecode.Label]

  lazy val bytecode: Seq[(Bytecode.Label, Bytecode)] =
    bytecodeSeq.zipWithIndex.map { case (b, i) => Bytecode.Label(i) -> b }

  def bytecodeFromLabel(l: Bytecode.Label): Bytecode =
    labelToBytecode(l)

  def pretty: String

  def methodReferences: Set[(ClassRef, MethodRef)] =
    bytecode.collect { case bc: Bytecode.HasMethodRef => (bc.classRef -> bc.methodRef) }.toSet

  def fieldReferences: Set[(ClassRef, FieldRef)] =
    bytecode.collect { case bc: Bytecode.HasFieldRef => (bc.classRef -> bc.fieldRef) }.toSet

  lazy val labelToBytecode: Map[Bytecode.Label, Bytecode] =
    bytecode.toMap

  def prepend(cf: CodeFragment): CodeFragment =
    cf + this

  def complete(): CodeFragment.Complete

  def +(rhs: CodeFragment): CodeFragment.Incomplete =
    this.concatForm + rhs

  def name(n: String): CodeFragment.Incomplete =
    CodeFragment.name(this, n)

  protected def concatForm: CodeFragment.Concat =
    CodeFragment.Concat(Seq(this), Map())
}
object CodeFragment {
  def bytecode(bcs: Bytecode*): CodeFragment.Complete =
    CodeFragment.Complete(bcs, Map.empty)

  def name(cf: CodeFragment, n: String): Incomplete =
    Concat(Seq(cf), Map(n -> Bytecode.Label(0)))

  def abstractJump(bc: Bytecode.HasAJumpTarget, name: String): CodeFragment =
    Partial(Seq(bc), Map((Bytecode.Label(0), bc.jumpTarget) -> Left(name)), Map.empty)

  def empty(): CodeFragment =
    new Complete(Seq.empty, Map.empty)

  case class Complete(
    bytecodeSeq: Seq[Bytecode],
    jumpTargets: Map[(Bytecode.Label, JumpTarget), Bytecode.Label]
  ) extends CodeFragment {
    {
      val jts = bytecode.collect {
        case (l, bc: Bytecode.HasJumpTargets) =>
          bc.jumpTargets.map(l -> _)
      }.flatten.toSet
      require(jts.size == jumpTargets.size)
      require(bytecode.forall {
        case (l, bc: Bytecode.HasJumpTargets) =>
          bc.jumpTargets.forall { jt => jumpTargets.contains(l -> jt) }
        case _ => true
      })
    }

    override def size = bytecodeSeq.size

    override def nameToLabel = Map.empty

    override def incompleteJumpTargets: Map[(Bytecode.Label, JumpTarget), Either[String, Bytecode.Label]] =
      jumpTargets.mapValues(Right(_))

    override def complete() = this

    override def pretty = Pretty.format_CodeFragment_Complete(this)

    def jumpDestination(bcl: Bytecode.Label, jt: JumpTarget): Bytecode.Label =
      jumpTargets(bcl -> jt)

    def rewrite_*(f: PartialFunction[(Bytecode.Label, Bytecode), CodeFragment]): CodeFragment.Incomplete =
      rewrite_** {
        case x @ (label, bc) if f.isDefinedAt(x) => Map(label -> f(x))
      }

    def rewrite_**(f: PartialFunction[(Bytecode.Label, Bytecode), Map[Bytecode.Label, CodeFragment]]): CodeFragment.Incomplete = {
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

    private[this] def rewrite0(rewrites: Map[Bytecode.Label, CodeFragment]): CodeFragment.Incomplete = {
      def adjustIndex(start: Int, shift: Int, l: Bytecode.Label) =
        if (l.index > start) l.offset(shift) else l

      val (bcs, jts, n2l,_) =
        rewrites.toSeq.sortBy(_._1.index).foldLeft(
          (bytecodeSeq, incompleteJumpTargets, Map.empty[String, Bytecode.Label], 0)
        ) {
          case ((bcs, jts, n2l, offset), (label, cf)) =>
            val start = label.index + offset
            require(cf.nonEmpty) // TODO: remove
            require(0 <= start && start < bcs.size)
            val newBcs = bcs.patch(start, cf.bytecodeSeq, 1)
            val shift = cf.bytecode.size - 1
            // TODO: [BUG] remove jumpTarget if replace target is jump
            val newJts: Map[(Bytecode.Label, JumpTarget), Either[String, Bytecode.Label]] =
              jts.map {
                case ((l, jt), dest) =>
                  val key = (adjustIndex(start, shift, l) -> jt)
                  key -> dest.fold(Left.apply, l => Right(adjustIndex(start, shift, l)))
              } ++ cf.incompleteJumpTargets.map { case ((l, jt), dest) =>
                (l.offset(start) -> jt) -> dest.fold(Left.apply, l => Right(l.offset(start)))
              }
            val newN2L = Algorithm.sharedNothingUnion(n2l, cf.nameToLabel.mapValues(_.offset(start))) getOrElse {
              throw new IllegalArgumentException(s"Name conflict: ${n2l.keys.filter(cf.nameToLabel.keySet).mkString(", ")}")
            }
            (newBcs, newJts, newN2L, offset + shift)
        }
      new CodeFragment.Partial(bcs, jts, n2l)
    }
  }

  sealed abstract class Incomplete extends CodeFragment {
    override def complete(): Complete
    override def nameToLabel: Map[String, Bytecode.Label]
  }

  case class Partial(
    override val bytecodeSeq: Seq[Bytecode],
    override val incompleteJumpTargets: Map[(Bytecode.Label, JumpTarget), Either[String, Bytecode.Label]],
    override val nameToLabel: Map[String, Bytecode.Label]
  ) extends Incomplete {
    override def size = bytecodeSeq.size
    override def pretty = toString // TODO
    override def complete() =
      new Complete(
        bytecodeSeq,
        incompleteJumpTargets.mapValues {
          case Left(name) => nameToLabel(name)
          case Right(label) => label
        }
      )
  }

  case class Concat(items: Seq[CodeFragment], additionalNameToLabel: Map[String, Bytecode.Label]) extends Incomplete {
    override val size = items.map(_.size).sum
    override def pretty = toString // TODO
    override lazy val bytecodeSeq = items.flatMap(_.bytecodeSeq)
    override lazy val (incompleteJumpTargets, nameToLabel) = {
      val (ijt, n2l, _) =
        items.foldLeft((
            Map.empty[(Bytecode.Label, JumpTarget), Either[String, Bytecode.Label]],
            additionalNameToLabel,
            0
        )) { case ((ijt, n2l, offset), cf) =>
          (
            Algorithm.sharedNothingUnion(
              ijt,
              cf.incompleteJumpTargets.map {
                case ((l, jt), dest) => (l.offset(offset) -> jt) -> dest.fold(Left.apply, l => Right(l.offset(offset)))
              }
            ).getOrElse { throw new AssertionError() },
            Algorithm.sharedNothingUnion(
              n2l,
              cf.nameToLabel.mapValues(_.offset(offset))
            ).getOrElse { throw new AssertionError() },
            offset + cf.size
          )
        };
      (
        ijt.mapValues {
          case Right(l) => Right(l)
          case Left(name) => n2l.get(name).map(Right.apply) getOrElse Left(name)
        },
        n2l
      )
    }
    override def complete(): Complete = {
      Partial(
        bytecodeSeq,
        incompleteJumpTargets,
        nameToLabel
      ).complete()
    }
    override def +(rhs: CodeFragment): Concat = rhs match {
      case Concat(rItems, an2l) =>
        val offset = items.map(_.size).sum
        Concat(
          items ++ rItems,
          Algorithm.sharedNothingUnion(
            additionalNameToLabel,
            an2l.mapValues { case l => l.offset(offset) }
          ).getOrElse {
            throw new IllegalArgumentException(s"Name conflict: ${additionalNameToLabel.keys.filter(an2l.keySet)}")
          }
        )
      case rhs => Concat(items :+ rhs, nameToLabel)
    }
    override def concatForm = this
  }
}

