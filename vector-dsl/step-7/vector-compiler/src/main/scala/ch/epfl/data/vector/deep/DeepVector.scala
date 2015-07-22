/* Generated by Purgatory 2014-2015 */

package ch.epfl.data.vector.deep

import ch.epfl.data.sc.pardis
import pardis.ir._
import pardis.types.PardisTypeImplicits._
import pardis.effects._
import pardis.deep._
import pardis.deep.scalalib._
import pardis.deep.scalalib.collection._
import pardis.deep.scalalib.io._
trait VectorOps extends Base with RichIntOps with SeqOps with NumericOps with IndexedSeqOps {
  // Type representation
  val VectorType = VectorIRs.VectorType
  implicit val typeVector: TypeRep[Vector] = VectorType
  implicit class VectorRep(self: Rep[Vector]) {
    def +(v2: Rep[Vector]): Rep[Vector] = vector$plus(self, v2)
    def *(v2: Rep[Vector]): Rep[Int] = vector$times(self, v2)
    def sameAs(v2: Rep[Vector]): Rep[Boolean] = vectorSameAs(self, v2)
    def data: Rep[Seq[Int]] = vector_Field_Data(self)
  }
  object Vector {
    def zero(n: Rep[Int]): Rep[Vector] = vectorZeroObject(n)
    def apply(data: Rep[Seq[Int]]): Rep[Vector] = vectorApplyObject(data)
  }
  // constructors
  def __newVector(data: Rep[Seq[Int]]): Rep[Vector] = vectorNew(data)
  // IR defs
  val VectorNew = VectorIRs.VectorNew
  type VectorNew = VectorIRs.VectorNew
  val Vector$plus = VectorIRs.Vector$plus
  type Vector$plus = VectorIRs.Vector$plus
  val Vector$times = VectorIRs.Vector$times
  type Vector$times = VectorIRs.Vector$times
  val VectorSameAs = VectorIRs.VectorSameAs
  type VectorSameAs = VectorIRs.VectorSameAs
  val Vector_Field_Data = VectorIRs.Vector_Field_Data
  type Vector_Field_Data = VectorIRs.Vector_Field_Data
  val VectorZeroObject = VectorIRs.VectorZeroObject
  type VectorZeroObject = VectorIRs.VectorZeroObject
  val VectorApplyObject = VectorIRs.VectorApplyObject
  type VectorApplyObject = VectorIRs.VectorApplyObject
  // method definitions
  def vectorNew(data: Rep[Seq[Int]]): Rep[Vector] = VectorNew(data)
  def vector$plus(self: Rep[Vector], v2: Rep[Vector]): Rep[Vector] = Vector$plus(self, v2)
  def vector$times(self: Rep[Vector], v2: Rep[Vector]): Rep[Int] = Vector$times(self, v2)
  def vectorSameAs(self: Rep[Vector], v2: Rep[Vector]): Rep[Boolean] = VectorSameAs(self, v2)
  def vector_Field_Data(self: Rep[Vector]): Rep[Seq[Int]] = Vector_Field_Data(self)
  def vectorZeroObject(n: Rep[Int]): Rep[Vector] = VectorZeroObject(n)
  def vectorApplyObject(data: Rep[Seq[Int]]): Rep[Vector] = VectorApplyObject(data)
  type Vector = ch.epfl.data.vector.shallow.Vector
}
object VectorIRs extends Base {
  import RichIntIRs._
  import SeqIRs._
  import NumericIRs._
  import IndexedSeqIRs._
  // Type representation
  case object VectorType extends TypeRep[Vector] {
    def rebuild(newArguments: TypeRep[_]*): TypeRep[_] = VectorType
    val name = "Vector"
    val typeArguments = Nil

    val typeTag = scala.reflect.runtime.universe.typeTag[Vector]
  }
  implicit val typeVector: TypeRep[Vector] = VectorType
  // case classes
  case class VectorNew(data: Rep[Seq[Int]]) extends ConstructorDef[Vector](List(), "Vector", List(List(data))) {
    override def curriedConstructor = (copy _)
  }

  case class Vector$plus(self: Rep[Vector], v2: Rep[Vector]) extends FunctionDef[Vector](Some(self), "+", List(List(v2))) {
    override def curriedConstructor = (copy _).curried
    override def isPure = true

    override def partiallyEvaluate(children: Any*): Vector = {
      val self = children(0).asInstanceOf[Vector]
      val v2 = children(1).asInstanceOf[Vector]
      self.$plus(v2)
    }
    override def partiallyEvaluable: Boolean = true

  }

  case class Vector$times(self: Rep[Vector], v2: Rep[Vector]) extends FunctionDef[Int](Some(self), "*", List(List(v2))) {
    override def curriedConstructor = (copy _).curried
    override def isPure = true

    override def partiallyEvaluate(children: Any*): Int = {
      val self = children(0).asInstanceOf[Vector]
      val v2 = children(1).asInstanceOf[Vector]
      self.$times(v2)
    }
    override def partiallyEvaluable: Boolean = true

  }

  case class VectorSameAs(self: Rep[Vector], v2: Rep[Vector]) extends FunctionDef[Boolean](Some(self), "sameAs", List(List(v2))) {
    override def curriedConstructor = (copy _).curried
    override def isPure = true

    override def partiallyEvaluate(children: Any*): Boolean = {
      val self = children(0).asInstanceOf[Vector]
      val v2 = children(1).asInstanceOf[Vector]
      self.sameAs(v2)
    }
    override def partiallyEvaluable: Boolean = true

  }

  case class Vector_Field_Data(self: Rep[Vector]) extends FieldDef[Seq[Int]](self, "data") {
    override def curriedConstructor = (copy _)
    override def isPure = true

    override def partiallyEvaluate(children: Any*): Seq[Int] = {
      val self = children(0).asInstanceOf[Vector]
      self.data
    }
    override def partiallyEvaluable: Boolean = true

  }

  case class VectorZeroObject(n: Rep[Int]) extends FunctionDef[Vector](None, "Vector.zero", List(List(n))) {
    override def curriedConstructor = (copy _)
    override def isPure = true

    override def partiallyEvaluate(children: Any*): Vector = {
      val n = children(0).asInstanceOf[Int]
      ch.epfl.data.vector.shallow.Vector.zero(n)
    }
    override def partiallyEvaluable: Boolean = true

  }

  case class VectorApplyObject(data: Rep[Seq[Int]]) extends FunctionDef[Vector](None, "Vector.apply", List(List(data))) {
    override def curriedConstructor = (copy _)
    override def isPure = true

    override def partiallyEvaluate(children: Any*): Vector = {
      val data = children(0).asInstanceOf[Seq[Int]]
      ch.epfl.data.vector.shallow.Vector.apply(data)
    }
    override def partiallyEvaluable: Boolean = true

  }

  type Vector = ch.epfl.data.vector.shallow.Vector
}
trait VectorImplicits extends VectorOps {
  // Add implicit conversions here!
}
trait VectorImplementations extends VectorOps {
  override def vector$plus(self: Rep[Vector], v2: Rep[Vector]): Rep[Vector] = {
    {
      val resultData: this.Rep[scala.collection.immutable.IndexedSeq[Int]] = intWrapper(unit(0)).until(self.data.size).map[Int, scala.collection.immutable.IndexedSeq[Int]](__lambda(((i: this.Rep[Int]) => self.data.apply(i).$plus(v2.data.apply(i)))));
      Vector.apply(resultData.toSeq)
    }
  }
  override def vector$times(self: Rep[Vector], v2: Rep[Vector]): Rep[Int] = {
    {
      var sum: this.Var[Int] = __newVarNamed(unit(0), unit("sum"));
      intWrapper(unit(0)).until(self.data.size).foreach[Unit](__lambda(((i: this.Rep[Int]) => __assign(sum, __readVar(sum).$plus(self.data.apply(i).$times(v2.data.apply(i)))))));
      __readVar(sum)
    }
  }
  override def vectorSameAs(self: Rep[Vector], v2: Rep[Vector]): Rep[Boolean] = {
    {
      var result: this.Var[Boolean] = __newVarNamed(unit(true), unit("result"));
      intWrapper(unit(0)).until(self.data.size).foreach[Unit](__lambda(((i: this.Rep[Int]) => __ifThenElse(infix_$bang$eq(self.data.apply(i), v2.data.apply(i)), __assign(result, unit(false)), unit(())))));
      __readVar(result)
    }
  }
  override def vectorZeroObject(n: Rep[Int]): Rep[Vector] = {
    __newVector(intWrapper(unit(0)).until(n).map[Int, scala.collection.immutable.IndexedSeq[Int]](__lambda(((x: this.Rep[Int]) => unit(0)))).toSeq)
  }
  override def vectorApplyObject(data: Rep[Seq[Int]]): Rep[Vector] = {
    __newVector(data)
  }
}

trait VectorPartialEvaluation extends VectorComponent with BasePartialEvaluation {
  // Immutable field inlining 
  override def vector_Field_Data(self: Rep[Vector]): Rep[Seq[Int]] = self match {
    case Def(node: VectorNew) => node.data
    case _                    => super.vector_Field_Data(self)
  }

  // Mutable field inlining 
  // Pure function partial evaluation
}
trait VectorComponent extends VectorOps with VectorImplicits {}
class VectorTransformation(override val IR: VectorOps with RichIntOps with SeqOps with NumericOps with IndexedSeqOps) extends pardis.optimization.RecursiveRuleBasedTransformer[VectorOps with RichIntOps with SeqOps with NumericOps with IndexedSeqOps](IR) {
  import IR.{ VectorRep => _, _ }
  type Rep[T] = IR.Rep[T]
  type Var[T] = IR.Var[T]

  private implicit class VectorRep1(self: Rep[Vector]) {

    def __newVector(data: Rep[Seq[Int]]): Rep[Vector] = IR.vectorNew(data)
    def +(v2: Rep[Vector]): Rep[Vector] = IR.vector$plus(self, v2)
    def *(v2: Rep[Vector]): Rep[Int] = IR.vector$times(self, v2)
    def sameAs(v2: Rep[Vector]): Rep[Boolean] = IR.vectorSameAs(self, v2)
    def data: Rep[Seq[Int]] = vector_Field_Data(self)
    def zero(n: Rep[Int]): Rep[Vector] = IR.vectorZeroObject(n)
    def apply(data: Rep[Seq[Int]]): Rep[Vector] = IR.vectorApplyObject(data)
  }
  def vector_Field_Data(self: Rep[Vector]): Rep[Seq[Int]] = field[Seq[Int]](self, "data")
  rewrite += statement {
    case sym -> (node @ VectorNew(nodedata)) if mustBeTransformed(sym) =>
      val data = nodedata.asInstanceOf[Rep[Seq[Int]]]

      val self = sym.asInstanceOf[Rep[Vector]]

      val _data = ("data", false, data)
      val newSelf = __new[Vector](_data)
      (newSelf)
  }

  def __newVector(data: Rep[Seq[Int]]): Rep[Vector] = VectorNew(data)

  rewrite += rule {
    case node @ Vector$plus(nodeself, nodev2) if mustBeTransformed(nodeself) =>

      val self = nodeself.asInstanceOf[Rep[Vector]]
      val v2 = nodev2.asInstanceOf[Rep[Vector]]

      {
        val resultData: this.Rep[scala.collection.immutable.IndexedSeq[Int]] = intWrapper(unit(0)).until(self.data.size).map[Int, scala.collection.immutable.IndexedSeq[Int]](__lambda(((i: this.Rep[Int]) => self.data.apply(i).$plus(v2.data.apply(i)))));
        Vector.apply(resultData.toSeq)
      }
  }

  rewrite += rule {
    case node @ Vector$times(nodeself, nodev2) if mustBeTransformed(nodeself) =>

      val self = nodeself.asInstanceOf[Rep[Vector]]
      val v2 = nodev2.asInstanceOf[Rep[Vector]]

      {
        var sum: this.Var[Int] = __newVarNamed(unit(0), unit("sum"));
        intWrapper(unit(0)).until(self.data.size).foreach[Unit](__lambda(((i: this.Rep[Int]) => __assign(sum, __readVar(sum).$plus(self.data.apply(i).$times(v2.data.apply(i)))))));
        __readVar(sum)
      }
  }

  rewrite += rule {
    case node @ VectorSameAs(nodeself, nodev2) if mustBeTransformed(nodeself) =>

      val self = nodeself.asInstanceOf[Rep[Vector]]
      val v2 = nodev2.asInstanceOf[Rep[Vector]]

      {
        var result: this.Var[Boolean] = __newVarNamed(unit(true), unit("result"));
        intWrapper(unit(0)).until(self.data.size).foreach[Unit](__lambda(((i: this.Rep[Int]) => __ifThenElse(infix_$bang$eq(self.data.apply(i), v2.data.apply(i)), __assign(result, unit(false)), unit(())))));
        __readVar(result)
      }
  }

  rewrite += rule {
    case node @ VectorZeroObject(noden) if mustBeTransformed(nodeself) =>

      val n = noden.asInstanceOf[Rep[Int]]

      __newVector(intWrapper(unit(0)).until(n).map[Int, scala.collection.immutable.IndexedSeq[Int]](__lambda(((x: this.Rep[Int]) => unit(0)))).toSeq)
  }

  rewrite += rule {
    case node @ VectorApplyObject(nodedata) if mustBeTransformed(nodeself) =>

      val data = nodedata.asInstanceOf[Rep[Seq[Int]]]

      __newVector(data)
  }

}
