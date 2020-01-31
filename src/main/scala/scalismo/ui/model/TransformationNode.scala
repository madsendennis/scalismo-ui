/*
 * Copyright (C) 2016  University of Basel, Graphics and Vision Research Group 
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

package scalismo.ui.model

import scalismo.geometry._3D
import scalismo.registration.RigidTransformation
import scalismo.ui.event.Event
import scalismo.ui.model.capabilities.{ Grouped, Removeable }

import scala.util.{ Failure, Success, Try }

object GenericTransformationsNode {

  object event {

    case class TransformationsChanged(source: TransformationCollectionNode) extends Event

  }

}

object ShapeModelTransformationsNode {

  object event {

    case class ShapeModelTransformationsChanged(source: ShapeModelTransformationsNode) extends Event

  }

}

object VolumeShapeModelTransformationsNode {

  object event {

    case class VolumeShapeModelTransformationsChanged(source: VolumeShapeModelTransformationsNode) extends Event

  }

}

object MoMoTransformationsNode {

  object event {

    case class MoMoTransformationsChanged(source: MoMoTransformationsNode) extends Event

  }

}

trait TransformationCollectionNode extends SceneNodeCollection[TransformationNode[_]] {

  val parent: GroupNode

  override protected def add(child: TransformationNode[_]): Unit = {
    listenTo(child)
    super.addToFront(child)
  }

}

class GenericTransformationsNode(override val parent: GroupNode) extends TransformationCollectionNode {
  override val name: String = "Generic transformations"

  def add[T <: PointTransformation](transformation: T, name: String): TransformationNode[T] = {
    val node = TransformationNode(this, transformation, name)
    add(node)
    node
  }

  def combinedTransformation: PointTransformation = {
    val transforms = children.map(_.transformation.asInstanceOf[PointTransformation])
    transforms.foldLeft(PointTransformation.Identity: PointTransformation) { case (first, second) => first compose second }
  }

  override protected def add(child: TransformationNode[_]): Unit = {
    super.add(child)
    publishEvent(GenericTransformationsNode.event.TransformationsChanged(this))
  }

  override def remove(child: TransformationNode[_]): Unit = {
    super.remove(child)
    publishEvent(GenericTransformationsNode.event.TransformationsChanged(this))
  }

  reactions += {
    case TransformationNode.event.TransformationChanged(_) =>
      publishEvent(GenericTransformationsNode.event.TransformationsChanged(this))
  }
}

class ShapeModelTransformationsNode(override val parent: GroupNode) extends TransformationCollectionNode with Removeable {
  override val name: String = "Shape model transformations"

  private def isPoseDefined: Boolean = {
    children.exists(tr => tr.transformation.isInstanceOf[RigidTransformation[_3D]])
  }

  private def isShapeDefined: Boolean = {
    children.exists(tr => tr.transformation.isInstanceOf[DiscreteLowRankGpPointTransformation])
  }

  def addPoseTransformation(transformation: RigidTransformation[_3D], name: String = "pose"): Try[ShapeModelTransformationComponentNode[RigidTransformation[_3D]]] = {

    if (isPoseDefined) {
      Failure(new Exception("The group already contains a rigid transformation as part of the Shape Model Transformation. Remove existing first"))
    } else {
      val node = ShapeModelTransformationComponentNode(this, transformation, name)
      add(node)
      Success(node)
    }
  }

  def addGaussianProcessTransformation(transformation: DiscreteLowRankGpPointTransformation, name: String = "shape"): Try[ShapeModelTransformationComponentNode[DiscreteLowRankGpPointTransformation]] = {

    if (isShapeDefined) {
      Failure(new Exception("The group already contains a GP transformation as part of the Shape Model Transformation. Remove existing first"))
    } else {
      val node = ShapeModelTransformationComponentNode(this, transformation, name)
      add(node)
      Success(node)
    }
  }

  def poseTransformation: Option[ShapeModelTransformationComponentNode[RigidTransformation[_3D]]] =
    children.find(_.transformation.isInstanceOf[RigidTransformation[_3D]]).map(_.asInstanceOf[ShapeModelTransformationComponentNode[RigidTransformation[_3D]]])

  def gaussianProcessTransformation: Option[ShapeModelTransformationComponentNode[DiscreteLowRankGpPointTransformation]] =
    children.find(_.transformation.isInstanceOf[DiscreteLowRankGpPointTransformation]).map(_.asInstanceOf[ShapeModelTransformationComponentNode[DiscreteLowRankGpPointTransformation]])

  protected def add(child: ShapeModelTransformationComponentNode[_]): Unit = {
    listenTo(child)
    super.addToFront(child)
    publishEvent(ShapeModelTransformationsNode.event.ShapeModelTransformationsChanged(this))
  }

  override def remove(child: TransformationNode[_]): Unit = {
    deafTo(child)
    super.remove(child)
    publishEvent(ShapeModelTransformationsNode.event.ShapeModelTransformationsChanged(this))
  }

  def combinedTransformation: Option[PointTransformation] = {
    gaussianProcessTransformation match {
      case Some(shapeTrans) => poseTransformation match {
        case Some(poseTrans) => Some(poseTrans.transformation compose shapeTrans.transformation)
        case None => Some(shapeTrans.transformation)
      }
      case None => poseTransformation match {
        case Some(poseTrans) => Some(poseTrans.transformation)
        case None => None
      }
    }
  }

  // in this case remove does not really remove the node from the parent group, but just empties its children
  def remove(): Unit = {
    children.foreach(_.remove())
  }

  reactions += {
    case TransformationNode.event.TransformationChanged(_) =>
      publishEvent(ShapeModelTransformationsNode.event.ShapeModelTransformationsChanged(this))
  }
}

class VolumeShapeModelTransformationsNode(override val parent: GroupNode) extends TransformationCollectionNode with Removeable {
  override val name: String = "Volume Shape model transformations"

  private def isPoseDefined: Boolean = {
    children.exists(tr => tr.transformation.isInstanceOf[RigidTransformation[_3D]])
  }

  private def isShapeDefined: Boolean = {
    children.exists(tr => tr.transformation.isInstanceOf[DiscreteLowRankGpPointTransformation])
  }

  def addPoseTransformation(transformation: RigidTransformation[_3D], name: String = "pose"): Try[VolumeShapeModelTransformationComponentNode[RigidTransformation[_3D]]] = {

    if (isPoseDefined) {
      Failure(new Exception("The group already contains a rigid transformation as part of the Shape Model Transformation. Remove existing first"))
    } else {
      val node = VolumeShapeModelTransformationComponentNode(this, transformation, name)
      add(node)
      Success(node)
    }
  }

  def addGaussianProcessTransformation(transformation: DiscreteLowRankGpPointTransformation, name: String = "shape"): Try[VolumeShapeModelTransformationComponentNode[DiscreteLowRankGpPointTransformation]] = {

    if (isShapeDefined) {
      Failure(new Exception("The group already contains a GP transformation as part of the Shape Model Transformation. Remove existing first"))
    } else {
      val node = VolumeShapeModelTransformationComponentNode(this, transformation, name)
      add(node)
      Success(node)
    }
  }

  def poseTransformation: Option[VolumeShapeModelTransformationComponentNode[RigidTransformation[_3D]]] =
    children.find(_.transformation.isInstanceOf[RigidTransformation[_3D]]).map(_.asInstanceOf[VolumeShapeModelTransformationComponentNode[RigidTransformation[_3D]]])

  def gaussianProcessTransformation: Option[VolumeShapeModelTransformationComponentNode[DiscreteLowRankGpPointTransformation]] =
    children.find(_.transformation.isInstanceOf[DiscreteLowRankGpPointTransformation]).map(_.asInstanceOf[VolumeShapeModelTransformationComponentNode[DiscreteLowRankGpPointTransformation]])

  protected def add(child: VolumeShapeModelTransformationComponentNode[_]): Unit = {
    listenTo(child)
    super.addToFront(child)
    publishEvent(VolumeShapeModelTransformationsNode.event.VolumeShapeModelTransformationsChanged(this))
  }

  override def remove(child: TransformationNode[_]): Unit = {
    deafTo(child)
    super.remove(child)
    publishEvent(VolumeShapeModelTransformationsNode.event.VolumeShapeModelTransformationsChanged(this))
  }

  def combinedTransformation: Option[PointTransformation] = {
    gaussianProcessTransformation match {
      case Some(shapeTrans) => poseTransformation match {
        case Some(poseTrans) => Some(poseTrans.transformation compose shapeTrans.transformation)
        case None => Some(shapeTrans.transformation)
      }
      case None => poseTransformation match {
        case Some(poseTrans) => Some(poseTrans.transformation)
        case None => None
      }
    }
  }

  // in this case remove does not really remove the node from the parent group, but just empties its children
  def remove(): Unit = {
    children.foreach(_.remove())
  }

  reactions += {
    case TransformationNode.event.TransformationChanged(_) =>
      publishEvent(VolumeShapeModelTransformationsNode.event.VolumeShapeModelTransformationsChanged(this))
  }
}

class MoMoTransformationsNode(override val parent: GroupNode) extends TransformationCollectionNode with Removeable {
  override val name: String = "MoMo transformations"

  private def isPoseDefined: Boolean = {
    children.exists(tr => tr.transformation.isInstanceOf[RigidTransformation[_3D]])
  }

  private def isShapeDefined: Boolean = {
    children.exists(tr => tr.transformation.isInstanceOf[DiscreteLowRankGpPointTransformation] && tr.name == "MoMoShape")
  }

  private def isColorDefined: Boolean = {
    children.exists(tr => tr.transformation.isInstanceOf[DiscreteLowRankGpPointTransformation] && tr.name == "MoMoColor")
  }

  def addPoseTransformation(transformation: RigidTransformation[_3D], name: String = "pose"): Try[MoMoTransformationComponentNode[RigidTransformation[_3D]]] = {
    if (isPoseDefined) {
      Failure(new Exception("The group already contains a rigid transformation as part of the MoMo Transformation. Remove existing first"))
    } else {
      val node = MoMoTransformationComponentNode(this, transformation, name)
      add(node)
      Success(node)
    }
  }

  def addShapeGaussianProcessTransformation(transformation: DiscreteLowRankGpPointTransformation, name: String = "MoMoShape"): Try[MoMoTransformationComponentNode[DiscreteLowRankGpPointTransformation]] = {

    if (isShapeDefined) {
      Failure(new Exception("The group already contains a Shape GP transformation as part of the MoMo Transformation. Remove existing first"))
    } else {
      val node = MoMoTransformationComponentNode(this, transformation, name)
      add(node)
      Success(node)
    }
  }

  def addColorGaussianProcessTransformation(transformation: DiscreteLowRankGpColorTransformation, name: String = "MoMoColor"): Try[MoMoTransformationComponentNode[DiscreteLowRankGpColorTransformation]] = {

    if (isColorDefined) {
      Failure(new Exception("The group already contains a Color GP transformation as part of the MoMo Transformation. Remove existing first"))
    } else {
      val node = MoMoTransformationComponentNode(this, transformation, name)
      add(node)
      Success(node)
    }
  }

  def poseTransformation: Option[MoMoTransformationComponentNode[RigidTransformation[_3D]]] =
    children.find(_.transformation.isInstanceOf[RigidTransformation[_3D]]).map(_.asInstanceOf[MoMoTransformationComponentNode[RigidTransformation[_3D]]])

  def gaussianShapeProcessTransformation: Option[MoMoTransformationComponentNode[DiscreteLowRankGpPointTransformation]] =
    children.find(tr => tr.transformation.isInstanceOf[DiscreteLowRankGpPointTransformation] && tr.name == "MoMoShape").map(_.asInstanceOf[MoMoTransformationComponentNode[DiscreteLowRankGpPointTransformation]])

  def gaussianColorProcessTransformation: Option[MoMoTransformationComponentNode[DiscreteLowRankGpColorTransformation]] =
    children.find(tr => tr.transformation.isInstanceOf[DiscreteLowRankGpColorTransformation] && tr.name == "MoMoColor").map(_.asInstanceOf[MoMoTransformationComponentNode[DiscreteLowRankGpColorTransformation]])

  protected def add(child: MoMoTransformationComponentNode[_]): Unit = {
    listenTo(child)
    super.addToFront(child)
    publishEvent(MoMoTransformationsNode.event.MoMoTransformationsChanged(this))
  }

  override def remove(child: TransformationNode[_]): Unit = {
    deafTo(child)
    super.remove(child)
    publishEvent(MoMoTransformationsNode.event.MoMoTransformationsChanged(this))
  }

  def combinedTransformation: Option[PointTransformation] = {
    gaussianColorProcessTransformation match {
      case Some(colorTrans) => poseTransformation match {
        case Some(poseTrans) => Some(poseTrans.transformation compose colorTrans.transformation)
        case None => Some(colorTrans.transformation)
      }
      case None => poseTransformation match {
        case Some(poseTrans) => Some(poseTrans.transformation)
        case None => None
      }
    }
  }

//  def combinedTransformation: Option[PointTransformation] = {
//    gaussianShapeProcessTransformation match {
//      case Some(shapeTrans) => poseTransformation match {
//        case Some(poseTrans) => Some(poseTrans.transformation compose shapeTrans.transformation)
//        case None => Some(shapeTrans.transformation)
//      }
//      case None => poseTransformation match {
//        case Some(poseTrans) => Some(poseTrans.transformation)
//        case None => None
//      }
//    }
//  }

  // in this case remove does not really remove the node from the parent group, but just empties its children
  def remove(): Unit = {
    children.foreach(_.remove())
  }

  reactions += {
    case TransformationNode.event.TransformationChanged(_) =>
      publishEvent(MoMoTransformationsNode.event.MoMoTransformationsChanged(this))
  }


}

class ShapeModelTransformationComponentNode[T <: PointTransformation] private (override val parent: ShapeModelTransformationsNode, initialTransformation: T, override val name: String)
    extends TransformationNode[T](parent, initialTransformation, name) {
  override def remove(): Unit = {
    parent.remove(this)
  }
}

object ShapeModelTransformationComponentNode {
  def apply(parent: ShapeModelTransformationsNode, initialTransformation: RigidTransformation[_3D], name: String) = new ShapeModelTransformationComponentNode(parent, initialTransformation, name)

  def apply(parent: ShapeModelTransformationsNode, initialTransformation: DiscreteLowRankGpPointTransformation, name: String) = new ShapeModelTransformationComponentNode(parent, initialTransformation, name)
}

class VolumeShapeModelTransformationComponentNode[T <: PointTransformation] private (override val parent: VolumeShapeModelTransformationsNode, initialTransformation: T, override val name: String)
    extends TransformationNode[T](parent, initialTransformation, name) {
  override def remove(): Unit = {
    parent.remove(this)
  }
}

object VolumeShapeModelTransformationComponentNode {
  def apply(parent: VolumeShapeModelTransformationsNode, initialTransformation: RigidTransformation[_3D], name: String) = new VolumeShapeModelTransformationComponentNode(parent, initialTransformation, name)

  def apply(parent: VolumeShapeModelTransformationsNode, initialTransformation: DiscreteLowRankGpPointTransformation, name: String) = new VolumeShapeModelTransformationComponentNode(parent, initialTransformation, name)
}

class MoMoTransformationComponentNode[T <: PointTransformation] private (override val parent: MoMoTransformationsNode, initialTransformation: T, override val name: String)
  extends TransformationNode[T](parent, initialTransformation, name) {
  override def remove(): Unit = {
    parent.remove(this)
  }
}

object MoMoTransformationComponentNode {
  def apply(parent: MoMoTransformationsNode, initialTransformation: RigidTransformation[_3D], name: String) = new MoMoTransformationComponentNode(parent, initialTransformation, name)

  def apply(parent: MoMoTransformationsNode, initialTransformation: DiscreteLowRankGpPointTransformation, name: String) = new MoMoTransformationComponentNode(parent, initialTransformation, name)

  def apply(parent: MoMoTransformationsNode, initialTransformation: DiscreteLowRankGpColorTransformation, name: String) = new MoMoTransformationComponentNode(parent, initialTransformation, name)
}

object TransformationNode {
  def apply[T <: PointTransformation](parent: TransformationCollectionNode, transformation: T, name: String): TransformationNode[T] = {
    new TransformationNode(parent, transformation, name)
  }

  object event {

    case class TransformationChanged[T <: PointTransformation](source: TransformationNode[T]) extends Event

  }

}

class TransformationNode[T <: PointTransformation](override val parent: TransformationCollectionNode, initialTransformation: T, override val name: String) extends SceneNode with Grouped with Removeable {
  private var _transformation: T = initialTransformation

  def transformation: T = _transformation

  def transformation_=(newTransformation: T): Unit = {
    _transformation = newTransformation
    publishEvent(TransformationNode.event.TransformationChanged(this))
  }

  override def remove(): Unit = parent.remove(this)

  override def group: GroupNode = parent.parent
}

