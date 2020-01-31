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

import scalismo.color.{RGB, RGBA}
import scalismo.common.UnstructuredPointsDomain
import scalismo.faces.momo.MoMo
import scalismo.geometry.{EuclideanVector, Point, _3D}
import scalismo.mesh.{SurfacePointProperty, VertexColorMesh3D}
import scalismo.statisticalmodel.{DiscreteLowRankGaussianProcess, StatisticalMeshModel, StatisticalVolumeMeshModel}
import scalismo.ui.api.ShapeModelTransformation
import scalismo.ui.api.ShowInScene.ShowVertexColorMesh
import scalismo.ui.event.ScalismoPublisher
import scalismo.ui.model.Scene.event.SceneChanged
import scalismo.ui.model.capabilities.{Removeable, Renameable}

class GroupsNode(override val parent: Scene) extends SceneNodeCollection[GroupNode] {
  override val name = "Groups"

  def add(name: String, hidden: Boolean = false): GroupNode = {
    val node = new GroupNode(this, name, hidden)
    add(node)
    node
  }

  // the groups node is always collapsed in the view.
  override def isViewCollapsed: Boolean = true
}

class GroupNode(override val parent: GroupsNode, initialName: String, initallyHidden: Boolean) extends SceneNode with Renameable with Removeable with ScalismoPublisher {
  name = initialName

  private var _hidden = initallyHidden

  def hidden_=(b: Boolean): Unit = {
    _hidden = b
    scene.publishEvent(SceneChanged(scene))
  }

  def hidden: Boolean = _hidden

  val genericTransformations = new GenericTransformationsNode(this)
  val shapeModelTransformations = new ShapeModelTransformationsNode(this)
  val volumeShapeModelTransformations = new VolumeShapeModelTransformationsNode(this)
  val momoTransformations = new MoMoTransformationsNode(this)

  val landmarks = new LandmarksNode(this)
  val triangleMeshes = new TriangleMeshesNode(this)
  val colorMeshes = new VertexColorMeshesNode(this)
  val scalarMeshFields = new ScalarMeshFieldsNode(this)
  val lineMeshes = new LineMeshesNode(this)
  val vectorFields = new VectorFieldsNode(this)
  val pointClouds = new PointCloudsNode(this)
  val images = new ImagesNode(this)
  val scalarFields = new ScalarFieldsNode(this)
  val tetrahedralMeshes = new TetrahedralMeshesNode(this)
  val tetrahedralMeshFields = new ScalarTetrahedralMeshFieldsNode(this)

  override val children: List[SceneNode] = List(
    genericTransformations,
    shapeModelTransformations,
    volumeShapeModelTransformations,
    momoTransformations,
    landmarks,
    triangleMeshes,
    colorMeshes,
    lineMeshes,
    scalarMeshFields,
    pointClouds,
    images,
    scalarFields,
    vectorFields,
    tetrahedralMeshes,
    tetrahedralMeshFields
  )

  // this is a convenience method to add a statistical model as a (gp, mesh) combination.
  def addStatisticalMeshModel(model: StatisticalMeshModel, initialName: String): Unit = {
    // FIXME: this method does not check the return values of the shapeModelTransformations.add(*) methods.
    // If another SSM already exists in the same group, this is very likely to yield unexpected results (but without failing or indicating an error).
    // This method should either be replaced with a safer implementation, or maybe be removed altogether in favor of a different solution.
    //
    // NOTE: the following code:
    // genericTransformations.add(DiscreteLowRankGpPointTransformation(model.gp), initialName)
    // is not a satisfactory solution IMO, but (at best) a semi-functional workaround.

    triangleMeshes.add(model.referenceMesh, initialName)
    shapeModelTransformations.addPoseTransformation(PointTransformation.RigidIdentity)
    shapeModelTransformations.addGaussianProcessTransformation(DiscreteLowRankGpPointTransformation(model.gp))

  }

  def addStatisticalVolumeMeshModel(model: StatisticalVolumeMeshModel, initialName: String): Unit = {

    tetrahedralMeshes.add(model.referenceVolumeMesh, initialName)
    volumeShapeModelTransformations.addPoseTransformation(PointTransformation.RigidIdentity)
    volumeShapeModelTransformations.addGaussianProcessTransformation(DiscreteLowRankGpPointTransformation(model.gp))

  }

  def addMoMo(model: MoMo, initialName: String): Unit = {

    val vcol = model.neutralModel.color.mean.data.seq.map(RGBA(_))
    val col: SurfacePointProperty[RGBA] = SurfacePointProperty(model.referenceMesh.triangulation, vcol)
    val referenceColor = VertexColorMesh3D(model.referenceMesh, col)
    colorMeshes.add(referenceColor, initialName)

//    triangleMeshes.add(model.referenceMesh, initialName)

    val momoGPshape: DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], Point[_3D]] = model.neutralModel.shape.gpModel
    val momoGPcolor: DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], RGB] = model.neutralModel.color.gpModel

    val gpShape: DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], EuclideanVector[_3D]] =
      new DiscreteLowRankGaussianProcess(momoGPshape.domain, momoGPshape.meanVector, momoGPshape.variance, momoGPshape.basisMatrix)

    val gpColor: DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], EuclideanVector[_3D]] =
      new DiscreteLowRankGaussianProcess(momoGPcolor.domain, momoGPcolor.meanVector, momoGPcolor.variance, momoGPcolor.basisMatrix)

    //    val shapeModelTransform = ShapeModelTransformation(PointTransformation.RigidIdentity, gpShape)

    momoTransformations.addPoseTransformation(PointTransformation.RigidIdentity)
    momoTransformations.addShapeGaussianProcessTransformation(DiscreteLowRankGpPointTransformation(gpShape))
    momoTransformations.addColorGaussianProcessTransformation(DiscreteLowRankGpColorTransformation(gpColor))
  }

  override def remove(): Unit = parent.remove(this)
}

