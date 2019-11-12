
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

package scalismo.ui.rendering.actor

import scalismo.geometry._3D
import scalismo.tetramesh.TetrahedralMesh
import scalismo.ui.model._
import scalismo.ui.model.capabilities.Transformable
import scalismo.ui.model.properties._
import scalismo.ui.rendering.Caches
import scalismo.ui.rendering.Caches.FastCachingTetrahedralMesh
import scalismo.ui.rendering.actor.TetrahedralActor.TetrahedralRenderable
import scalismo.ui.rendering.actor.mixin._
import scalismo.ui.rendering.util.VtkUtil
import scalismo.ui.view.{ViewportPanel, ViewportPanel2D, ViewportPanel3D}
import scalismo.utils.TetraMeshConversion
import vtk.{vtkActor, vtkPolyData, vtkPolyDataNormals, vtkUnstructuredGrid}

object TetrahedralMeshActor extends SimpleActorsFactory[TetrahedralMeshNode] {

  override def actorsFor(renderable: TetrahedralMeshNode, viewport: ViewportPanel): Option[Actors] = {
    viewport match {
      case _: ViewportPanel3D => Some(new TetrahedralMeshActor3D(renderable))
      case _2d: ViewportPanel2D => Some(new TetrahedralMeshActor2D(renderable, _2d))
    }
  }
}

//object ScalarTetrahedralMeshFieldActor extends SimpleActorsFactory[ScalarMeshFieldNode] {
//
//  override def actorsFor(renderable: ScalarMeshFieldNode, viewport: ViewportPanel): Option[Actors] = {
//    viewport match {
//      case _: ViewportPanel3D => Some(new ScalarMeshFieldActor3D(renderable))
//      case _2d: ViewportPanel2D => Some(new ScalarMeshFieldActor2D(renderable, _2d))
//    }
//  }
//}
//
//object VertexColorTetrahedralMeshActor extends SimpleActorsFactory[VertexColorMeshNode] {
//  override def actorsFor(renderable: VertexColorMeshNode, viewport: ViewportPanel): Option[Actors] = {
//    viewport match {
//      case _: ViewportPanel3D => Some(new VertexColorMeshActor3D(renderable))
//      case _2d: ViewportPanel2D => Some(new VertexColorMeshActor2D(renderable, _2d))
//    }
//  }
//}

object TetrahedralActor {

  trait TetrahedralRenderable {

    type MeshType

    def opacity: OpacityProperty

    def lineWidth: LineWidthProperty

    def mesh: MeshType

    def node: SceneNode
  }

  private[actor] object TetrahedralRenderable {

    class TetrahedralMeshRenderable(override val node: TetrahedralMeshNode) extends TetrahedralRenderable {

      type MeshType = TetrahedralMesh[_3D]

      override def mesh: TetrahedralMesh[_3D] = node.transformedSource

      override def opacity: OpacityProperty = node.opacity

      override def lineWidth: LineWidthProperty = node.lineWidth

      def color: ColorProperty = node.color
    }


    def apply(source: TetrahedralMeshNode): TetrahedralMeshRenderable = new TetrahedralMeshRenderable(source)

  }

}

trait TetrahedralActor[R <: TetrahedralRenderable] extends ActorOpacity with ActorSceneNode {
  def renderable: R

  //val mapper2 = new vtk.vtkDataSetMapper()

  override def opacity: OpacityProperty = renderable.opacity

  override def sceneNode: SceneNode = renderable.node

  protected def meshToUnstructuredGrid(template: Option[vtkUnstructuredGrid]): vtkUnstructuredGrid

  protected var unstructuredgrid: vtkUnstructuredGrid = meshToUnstructuredGrid(None)

  // this is invoked from within the rerender method, if the geometry has changed.
  protected def onGeometryChanged(): Unit

  protected def rerender(geometryChanged: Boolean): Unit = {
    if (geometryChanged) {
      unstructuredgrid = meshToUnstructuredGrid(Some(unstructuredgrid))
      onGeometryChanged()
    }

    actorChanged(geometryChanged)
  }

  protected def onInstantiated(): Unit = {}

  //FIXME: pick control -- this should probably go into a trait or something.
  renderable.node match {
    case p: HasPickable =>
      SetPickable(if (p.pickable.value) 1 else 0)
      listenTo(p.pickable)
      reactions += {
        case NodeProperty.event.PropertyChanged(s) if s == p.pickable =>
          SetPickable(if (p.pickable.value) 1 else 0)
      }
    case _ =>
  }

  onInstantiated()

  rerender(geometryChanged = true)

  listenTo(renderable.node)

  reactions += {
    case Transformable.event.GeometryChanged(_) => rerender(geometryChanged = true)
  }

}

trait TetrahedralMeshActor extends TetrahedralActor[TetrahedralRenderable.TetrahedralMeshRenderable] {

  override def renderable: TetrahedralRenderable.TetrahedralMeshRenderable

  override protected def meshToUnstructuredGrid(template: Option[vtkUnstructuredGrid]): vtkUnstructuredGrid = {
    TetraMeshConversion.tetrameshTovtkUnstructuredGrid(renderable.mesh)
    //Caches.TetrahedralMeshCache.getOrCreate(FastCachingTetrahedralMesh(renderable.mesh), TetraMeshConversion.tetrameshTovtkUnstructuredGrid(renderable.mesh))
  }
}

abstract class TetrahedralActor3D[R <: TetrahedralRenderable](override val renderable: R) extends UnstructuredGridActor with TetrahedralActor[R] {

  // not declaring this as lazy causes all sorts of weird VTK errors, probably because the methods which use
  // it are invoked from the superclass constructor (at which time this class is not necessarily fully initialized)(?)
  //private lazy val normals: vtkPolyDataNormals = new vtk.vtkPolyDataNormals() {
    //ComputePointNormalsOn()
    //ComputeCellNormalsOff()
  //}

//  val t = new vtk.vtkDataSetSurfaceFilter()
//  t.AddInputData(unstructuredgrid)
//  t.Update()
//  val polydata: vtkPolyData = t.GetOutput()
//  val normals: vtkPolyDataNormals = new vtk.vtkPolyDataNormals(){
//        ComputePointNormalsOn()
//        ComputeCellNormalsOff()
//      }
//  normals.AddInputData(polydata)
//  normals.Update()


  override protected def onInstantiated(): Unit = {
//    mapper.SetInputConnection(normals.GetOutputPort())
    mapper.SetInputData(unstructuredgrid)
  }

  override protected def onGeometryChanged(): Unit = {
//    normals.RemoveAllInputs()
//    normals.SetInputData(unstructuredgrid)
//    normals.Update()
  }

}

abstract class TetrahedralActor2D[R <: TetrahedralRenderable](override val renderable: R, viewport: ViewportPanel2D) extends SlicingActor(viewport) with TetrahedralActor[R] with ActorLineWidth {
  override def lineWidth: LineWidthProperty = renderable.lineWidth

  override protected def onSlicingPositionChanged(): Unit = rerender(geometryChanged = false)

  override protected def onGeometryChanged(): Unit = {
    planeCutter.SetInputData(unstructuredgrid)
    planeCutter.Modified()
  }

  override protected def sourceBoundingBox: BoundingBox = VtkUtil.bounds2BoundingBox(unstructuredgrid.GetBounds())
}


class TetrahedralMeshActor3D(node: TetrahedralMeshNode) extends TetrahedralActor3D(TetrahedralRenderable(node)) with  TetrahedralMeshActor

class TetrahedralMeshActor2D(node: TetrahedralMeshNode, viewport: ViewportPanel2D) extends TetrahedralActor2D(TetrahedralRenderable(node), viewport) with TetrahedralMeshActor
//
//class UnstructuredGridActor extends vtkActor {
//  val mapper = new vtk.vtkDataSetMapper()
//  //
//  //  // to set a Blue to Red Color map
//  //  val lut = new vtkLookupTable()
//  //  lut.SetHueRange(0.667, 0.0)
//  //  lut.SetNumberOfColors(256)
//  //  lut.Build()
//  //  mapper.SetLookupTable(lut)
//
//  SetMapper(mapper)
//  GetProperty().SetInterpolationToGouraud()
//
//}
