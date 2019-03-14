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
    val size = A.length
    // Use Gauss-Method to get the inverse
    // Here's Java code
    // http://cljavacode.blogspot.com/2017/06/inverse-matrix-by-gauss-jordan.html

    // Create an augmented matrix and add an identity matrix
    // at the right side of the original matrix
    var augA: Array[Array[Double]] = Array.ofDim(size*2, size)
    for 
    {
      colIndex <- 0 until 2*size
      rowIndex <- 0 until size
    }
    {
      if (colIndex >= size)
      {
        if (colIndex == rowIndex)
          augA(colIndex)(rowIndex) = 1.0
        else
          augA(colIndex)(rowIndex) = 0.0
      }
      else
        augA(colIndex)(rowIndex) = A(colIndex)(rowIndex)
    }
    
   
    
    // replace a row by a sum of itself and a constant multiple
    // of another row of the matrix
    for 
    {
      colIndex <- 0 until 2*size
      rowIndex <- 0 until size
    }
    {
      if (colIndex != rowIndex)
      {
        val temporary = augA(colIndex)(rowIndex) / augA(rowIndex)(rowIndex)
        for (k <- 0 until 2*size)
          augA(k)(rowIndex) -= augA(k)(rowIndex)*temporary
      }
    }
    
    // Multiply each row by a nonzero integer
    // devide row element by the diagonal element
    for (rowIndex <- 0 until size)
    {
      val temporary = augA(rowIndex)(rowIndex)
      for (colIndex <- 0 until 2*size)
        augA(colIndex)(rowIndex) = augA(colIndex)(rowIndex)*temporary
    }
    
    var inverseA: Array[Array[Double]] = Array.ofDim(size, size) 
    for 
    {
      colIndex <- size until 2*size
      rowIndex <- 0 until size
    }
    {
      inverseA(colIndex-size)(rowIndex) = augA(colIndex)(rowIndex)      
    }
    return inverseA
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