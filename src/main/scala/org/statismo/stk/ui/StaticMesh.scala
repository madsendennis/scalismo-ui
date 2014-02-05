package org.statismo.stk.ui

import org.statismo.stk.core.mesh.TriangleMesh
import java.io.File
import scala.util.Try
import org.statismo.stk.core.io.MeshIO
import org.statismo.stk.core.geometry.Point3D

object StaticMesh extends Loadable[StaticMesh] {
  val description = "Static Mesh"
  val fileExtensions = Seq("h5","vtk")
  
  def apply(file: File)(implicit scene: Scene): Try[StaticMesh] = {
    for {
      raw <-MeshIO.readMesh(file)
    } yield {
      val static = new StaticMesh(raw, None, file.getName())
      static
    }
  }
}

case class StaticMesh(override val triangleMesh: TriangleMesh, initialParent: Option[SceneTreeObjectContainer[ThreeDRepresentation]] = None, initialName: String = "(no name)")(implicit override val scene: Scene) extends Mesh {
  name = initialName
  override lazy val parent: SceneTreeObjectContainer[ThreeDRepresentation] = initialParent.getOrElse {
    val p = new StaticThreeDObject(Some(scene.statics), initialName)
    p.representations
  }
  parent.add(Seq(this))
  
  def addLandmarkAt(point: Point3D) = ??? 
}