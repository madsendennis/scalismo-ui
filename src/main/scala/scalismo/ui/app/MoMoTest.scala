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

package scalismo.ui.app

import java.io.File

import scalismo.mesh._
import scalismo.color._
import scalismo.common.UnstructuredPointsDomain
import scalismo.geometry._
import scalismo.utils.Random
import scalismo.faces.io._
import scalismo.faces.momo._
import scalismo.faces.mesh._
import scalismo.faces.image._
import scalismo.faces.parameters._
import scalismo.io.StatismoIO
import scalismo.ui.api.ScalismoUI
import vtk._

object MoMoTest {

  implicit val rng = Random(1024L)

  def main(args: Array[String]): Unit = {
    scalismo.initialize()
    println("Starting!")

    val modelURI = new File("data/model2017-1_face12_nomouth.h5").toURI
    val model: MoMo = MoMoIO.read(modelURI).get

//    val otherModel = StatismoIO.readStatismoMeshModel(new File("/home/madden00/workspace/uni/icp/icp-sampling-registration_cvpr/data/femur/femur_gp_model_50-components.h5")).get

    val ui = ScalismoUI("Scalismo Viewer")
    val grp = ui.createGroup("MoMo")
    val otherGropu = ui.createGroup("other")
    ui.show(grp, model, "momo")
//    val showOther = ui.show(otherGropu, otherModel, "other")
//    showOther.meshView.opacity = 0.0
    //    ui.show(volumeModel, "model")
    //    ui.show(volumeScalarMeshField, "scalarMeshField")
    println("All done!")
  }
}
