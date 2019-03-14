package regressionViz

class OLSModel (data: Data) extends Model (data)
{

  def fitData: Unit = 
  {
    // listwise deletion for missing values
    if (fittedData.isEmpty) checkAndDeleteMissingRows
    
    // check that there are enough rows and columns
    // after deleting rows with missing values
    require(fittedData.isDefined && 
        fittedData.get.getPoints.isDefined &&
        fittedData.get.getPoints.get.length > 1 &&
        fittedData.get.getPoints.get(0).length> 1,
        "There should be enough rows and columns " +
        "in the dataset after listwise deletion.")
    
    calculateNormalEquation
    calculateResiduals
  }
  
  protected def calculateNormalEquation =
  {
    // add the intercept to the equation (i.e. column of ones)
    // take the last column out = outcome variable
    val X: Array[Array[Double]] = 
    {  
      val originalPoints = fittedData.get.getPoints.get
      val originalLength = originalPoints(0).length
      val oneVector = Array.fill(originalLength)(1.0)
      val oneIncluded = oneVector +: originalPoints.dropRight(1)
      oneIncluded
    }
    // outcome variable is the last one
    val y: Array[Double] = fittedData.get.getPoints.get.last
    val Xt: Array[Array[Double]] = X.transpose
    val XtX: Array[Array[Double]] = multiplyMatrices(Xt, X)
    val Xty: Array[Double] = multiplyMatrixAndVector(Xt, y)
    val XtXInverse: Array[Array[Double]] = invertMatrix(XtX)
    val estimate: Array[Double] = multiplyMatrixAndVector(XtXInverse, Xty)
    equation = Some(estimate)
  }
  
  
  /*
   * helper functions for normal equation calculation
   */
  def multiplyMatrices(A: Array[Array[Double]], B: Array[Array[Double]]): 
    Array[Array[Double]] =
    {
      require(A.length == B(0).length)
      
      val commonLength = A.length
      val rowsC = A(0).length
      val colsC = B.length
      var C: Array[Array[Double]] = Array.ofDim(rowsC, colsC)
      
      for 
      {
        rowIndex <- 0 until rowsC
        colIndex <- 0 until colsC
      } 
      {
        var sumProduct: Double = 0.0
        for (commonIndex <- 0 until commonLength)
        {
          sumProduct += A(commonIndex)(rowIndex)*B(colIndex)(commonIndex)
        }
        C(colIndex)(rowIndex) = sumProduct
      }
      
      return C
    }
  
  def invertMatrix(A: Array[Array[Double]]): Array[Array[Double]] =
  {
    require(A.length == A(0).length)
    
    // Use Gauss-Method to get the inverse
    // Here's C++ code 
    // https://www.geeksforgeeks.org/finding-inverse-of-a-matrix-using-gauss-jordan-method/
    // Create an augmented matrix and add an identity matrix
    // at the right side of the original matrix
    
    // interchange the rows of the matrix
    
    // replace a row by a sum of itself and a constant multiple
    // of another row of the matrix
    A
  }
  
  def multiplyMatrixAndVector(A: Array[Array[Double]], x: Array[Double]):
    Array[Double] = 
  {
      require(A.length == x.length)
      
      val commonLength = A.length
      val rowsC = A(0).length
      var C: Array[Double] = Array.ofDim(rowsC)      
      for (rowIndex <- 0 until rowsC) 
      {
        var sumProduct: Double = 0.0
        for (commonIndex <- 0 until commonLength)
        {
          sumProduct += A(commonIndex)(rowIndex)*x(commonIndex)
        }
        C(rowIndex) = sumProduct
      }
      
      return C    
  }

  def calculateResiduals =
  {
    
  }
  
}