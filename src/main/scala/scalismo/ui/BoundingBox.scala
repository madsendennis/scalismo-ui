package scalismo.ui

object BoundingBox {

  private class DummyBoundingBox extends BoundingBox(0, 0, 0, 0, 0, 0) {
    override lazy val isDummy = true
  }

  val None: BoundingBox = new DummyBoundingBox
}

case class BoundingBox(xMin: Float, xMax: Float, yMin: Float, yMax: Float, zMin: Float, zMax: Float) {
  protected[BoundingBox] lazy val isDummy = false

  def union(that: BoundingBox): BoundingBox = {
    if (this.isDummy) {
      that
    } else if (that.isDummy) {
      this
    } else {
      BoundingBox(Math.min(this.xMin, that.xMin), Math.max(this.xMax, that.xMax), Math.min(this.yMin, that.yMin), Math.max(this.yMax, that.yMax), Math.min(this.zMin, that.zMin), Math.max(this.zMax, that.zMax))
    }
  }

  override def toString: String = {
    s"BoundingBox ($xMin -> $xMax)($yMin -> $yMax)($zMin -> $zMax) (dummy: $isDummy)"
  }
}