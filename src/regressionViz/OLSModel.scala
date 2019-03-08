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
    val ata: Array[Array[Double]] = transpose_And_Multiply_Itself
    val aty: Array[Double] = tranpose_And_Multiply_Outcome
    val ataInverse: Array[Array[Double]] = invertMatrix
    val estimate: Array[Double] = multiply_Matrix_And_Vector(ataInverse, aty)
    equation = Some(estimate)
  }
  
  def transpose_And_Multiply_Itself
  def tranpose_And_Multiply_Outcome
  def invertMatrix
  def multiplyMatrix(Array[Array[Double]], Array[Array[Double]]) = 
  {
    
  }

  def calculateResiduals =
  {
    
  }
  
}