package scalismo.ui.control.interactor.landmark.complex.posterior

import breeze.linalg.{ DenseMatrix, DenseVector }
import scalismo.geometry._
import scalismo.statisticalmodel.{ NDimensionalNormalDistribution, LowRankGaussianProcess }
import scalismo.statisticalmodel.LowRankGaussianProcess.Eigenpair
import scalismo.ui.control.interactor.landmark.complex.ComplexLandmarkingInteractor
import scalismo.ui.control.interactor.landmark.complex.ComplexLandmarkingInteractor.Delegate
import scalismo.ui.model._

trait PosteriorLandmarkingInteractor extends ComplexLandmarkingInteractor[PosteriorLandmarkingInteractor] {

  implicit val theFrame = frame

  def previewNode: TriangleMeshNode

  def sourceGpNode: TransformationNode[DiscreteLowRankGpPointTransformation]

  def previewGpNode: TransformationNode[DiscreteLowRankGpPointTransformation]

  def targetUncertaintyGroup: GroupNode

  def targetGroupNode: GroupNode

  override protected def initialDelegate: Delegate[PosteriorLandmarkingInteractor] = {
    PosteriorReadyForCreating.enter()
  }

  private def genericRegressionComputations(gp: LowRankGaussianProcess[_3D, _3D], trainingData: IndexedSeq[Point3D], modelLm: LandmarkNode, targetLm: LandmarkNode): DenseMatrix[Double] = {

    val dim = 3 //implicitly[NDSpace[DO]].dimensionality

    val xs = trainingData

    val Q = DenseMatrix.zeros[Double](trainingData.size * dim, gp.klBasis.size)
    for ((x_i, i) <- xs.zipWithIndex; (Eigenpair(lambda_j, phi_j), j) <- gp.klBasis.zipWithIndex) {
      Q(i * dim until i * dim + dim, j) := phi_j(x_i).toBreezeVector * math.sqrt(lambda_j)
    }

    val errorDistributions = IndexedSeq(NDimensionalNormalDistribution(scalismo.geometry.Vector(0f, 0f, 0f), modelLm.uncertainty.value.to3DNormalDistribution.cov + targetLm.uncertainty.value.to3DNormalDistribution.cov))
    // What we are actually computing here is the following:
    // L would be a block diagonal matrix, which contains on the diagonal the blocks that describes the uncertainty
    // for each point (a d x d) block. We then would compute Q.t * L. For efficiency reasons (L could be large but is sparse)
    // we avoid ever constructing the matrix L and do the multiplication by hand.
    val QtL = Q.t.copy
    assert(QtL.cols == errorDistributions.size * dim)
    assert(QtL.rows == gp.rank)
    for ((errDist, i) <- errorDistributions.zipWithIndex) {
      QtL(::, i * dim until (i + 1) * dim) := QtL(::, i * dim until (i + 1) * dim) * breeze.linalg.inv(errDist.cov.toBreezeMatrix)
    }

    val M = QtL * Q + DenseMatrix.eye[Double](gp.klBasis.size)
    val Minv = breeze.linalg.pinv(M)

    Minv * QtL
  }

  def updatePreview(modelLm: LandmarkNode, targetLm: LandmarkNode, mousePosition: Point3D): Unit = {

    targetUncertaintyGroup.transformations.foreach(_.remove())
    targetUncertaintyGroup.transformations.add((p: Point[_3D]) => mousePosition, "mousePosition")

    def flatten(v: IndexedSeq[Vector[_3D]]) = DenseVector(v.flatten(_.toArray).toArray)

    val lmPointAndId = {
      previewNode.source.pointSet.findClosestPoint(modelLm.source.point)
    }

    val mvec = flatten(IndexedSeq(sourceGpNode.transformation.dgp.mean(lmPointAndId.id)))
    val M = genericRegressionComputations(sourceGpNode.transformation.gp, IndexedSeq(lmPointAndId.point), modelLm, targetLm)

    val yvec = {
      val ys = IndexedSeq(lmPointAndId.id).map(id => mousePosition - lmPointAndId.point)
      flatten(ys)
    }

    val coefficients = M * (yvec - mvec)

    previewGpNode.transformation = sourceGpNode.transformation.copy(coefficients)
  }

  def showPreview(): Unit = {
    previewNode.visible = true
  }

  def hidePreview(): Unit = {
    previewNode.visible = false
  }

  def initialize(): Unit = {
    previewNode.pickable.value = false
    hidePreview()
  }
}

