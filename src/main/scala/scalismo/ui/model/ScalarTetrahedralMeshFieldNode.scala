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

import java.io.File

import scalismo.geometry.{Point, Point3D, _3D}
import scalismo.io.TetraMeshIO
import scalismo.tetramesh.{ScalarVolumeMeshField, TetrahedralMesh3D}
import scalismo.ui.model.capabilities._
import scalismo.ui.model.properties._
import scalismo.ui.util.{FileIoMetadata, FileUtil}

import scala.util.{Failure, Success, Try}

class ScalarTetrahedralMeshFieldsNode(override val parent: GroupNode) extends SceneNodeCollection[ScalarTetrahedralMeshFieldNode] with Loadable {
  override val name: String = "Scalar Tetrahedral Mesh Fields"

  override def loadMetadata: FileIoMetadata = FileIoMetadata.ScalarTetrahedralMeshField

  override def load(file: File): Try[Unit] = {
    val r = TetraMeshIO.readTetrahedralMesh(file) //todo change to read tetrahedral mesh field
    r match {
      case Failure(ex) => Failure(ex)
      case Success(mesh) =>
        val scalars = mesh.pointSet.points.map((pt: Point[_3D]) => 1.0.toFloat).toIndexedSeq //todo remove mesh to meshfield
        val volumeMeshField: ScalarVolumeMeshField[Float] = ScalarVolumeMeshField(mesh, scalars)
        add(volumeMeshField, FileUtil.basename(file))
        Success(())
    }
  }

  def add(mesh: ScalarVolumeMeshField[Float], name: String): ScalarTetrahedralMeshFieldNode = { //todo change to generic input instead of Float
    val node = new ScalarTetrahedralMeshFieldNode(this, mesh, name)
    add(node)
    node
  }
}

class ScalarTetrahedralMeshFieldNode(override val parent: ScalarTetrahedralMeshFieldsNode, override val source: ScalarVolumeMeshField[Float], initialName: String) extends Transformable[ScalarVolumeMeshField[Float]] with InverseTransformation with Saveable with Removeable with Renameable with HasOpacity with HasLineWidth with HasScalarRange {
  name = initialName

  override val opacity = new OpacityProperty()
  override val lineWidth = new LineWidthProperty()
  override val scalarRange: ScalarRangeProperty = {
    val (min, max) = (source.values.min, source.values.max)
    new ScalarRangeProperty(ScalarRange(min, max, min, max))
  }

  override def group: GroupNode = parent.parent

  override def remove(): Unit = parent.remove(this)

  override def inverseTransform(point: Point3D): Point3D = {
    val id = transformedSource.mesh.pointSet.findClosestPoint(point).id
    source.mesh.pointSet.point(id)
  }

  override def transform(untransformed: ScalarVolumeMeshField[Float], transformation: PointTransformation): ScalarVolumeMeshField[Float] = {
    untransformed.copy(mesh = untransformed.mesh.transform(transformation))
  }

  override def save(file: File): Try[Unit] = TetraMeshIO.writeTetraMesh(transformedSource.mesh, file) //TODO change writeTetraMesh to writeScalarTetraMesh when available

  override def saveMetadata: FileIoMetadata = FileIoMetadata.ScalarTetrahedralMeshField

}


