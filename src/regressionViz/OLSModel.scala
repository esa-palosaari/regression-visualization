package regressionViz

class OLSModel (data: Data) extends Model (data)
{

  def fitData: Unit = 
  {
    // listwise deletion for missing values
    if (fittedData.isEmpty) checkAndDeleteMissingRows
    
    // check that there are enough rows to fit
    require(fittedData.isDefined && 
        fittedData.get.getPoints.isDefined &&
        fittedData.get.getPoints.get.length > 1)
    
    calculateNormalEquation
    calculateResiduals
  }
  
  protected def calculateNormalEquation =
  {
    // add the intercept to the equation (i.e. column of ones)
    // take the last column out = outcome variable
    val X: Array[Array[Double]] = 
    {  
      val originalPoints = fittedData.get.getPoints
      val originalLength = originalPoints.get(0).length
      val oneVector = Array.fill(originalLength)(1.0)
      val newPoints = Some(oneVector +: originalPoints.get.dropRight(1))
      fittedData.get.initializeDataset(
          newPoints, 
          fittedData.get.name, 
          fittedData.get.getVarNames)
      fittedData.get.getPoints.get
    }
    // outcome variable is the last one
    val y: Array[Double] = fittedData.get.getPoints.get.last
    val Xt: Array[Array[Double]] = transposeMatrix(X)
    val XtX: Array[Array[Double]] = multiplyMatrices(Xt, X)
    val Xty: Array[Double] = multiplyMatrixAndVector(Xt, y)
    val XtXInverse: Array[Array[Double]] = invertMatrix(XtX)
    val estimate: Array[Double] = multiplyMatrixAndVector(XtXInverse, Xty)
    equation = Some(estimate)
  }
  
  def transposeMatrix(X: Array[Array[Double]]): Array[Array[Double]] =
  {
    X
  }
  
  def multiplyMatrices(A: Array[Array[Double]], B: Array[Array[Double]]): 
    Array[Array[Double]] =
    {
      A
    }
  
  def invertMatrix(A: Array[Array[Double]]): Array[Array[Double]] =
  {
    A
  }
  
  def multiplyMatrixAndVector(A: Array[Array[Double]], x: Array[Double]):
    Array[Double] = 
  {
    x
  }

  def calculateResiduals =
  {
    
  }
  
}