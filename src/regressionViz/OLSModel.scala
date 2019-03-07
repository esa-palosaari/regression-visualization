package regressionViz

class OLSModel (data: Data) extends Model (data)
{
  private var equation: Option[Array[Double]] = None
  private var residuals: Option[Array[Double]] = None
  
  def getEquation: Option[Array[Double]] = equation
  def getResiduals: Option[Array[Double]] = residuals

  def fitData: Unit = {
    equation = Some(Array(1,2))
    residuals = Some(Array(1, 2))
  }
}