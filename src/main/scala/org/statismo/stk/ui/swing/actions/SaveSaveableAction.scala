package org.statismo.stk.ui.swing.actions

import java.io.File

import scala.util.Try

import org.statismo.stk.ui.Saveable
import org.statismo.stk.ui.SceneTreeObject

class SaveSaveableAction extends SceneTreePopupAction("Save to file...") {
  def isContextSupported(context: Option[SceneTreeObject]) = {
    context.isDefined && context.get.isInstanceOf[Saveable] && context.get.asInstanceOf[Saveable].isCurrentlySaveable
  }

  override def apply(context: Option[SceneTreeObject]) = {
    if (isContextSupported(context)) {
      val save = context.get.asInstanceOf[Saveable]
      def doSave(file: File): Try[Unit] = {
        save.saveToFile(file)
      }
      new SaveAction(doSave, save.saveableMetadata).apply()
    }
  }
}