package regressionViz

class OLSModel (data: Data) extends Model (data)
{
  private var equation: Option[Array[Double]] = None
  private var residuals: Option[Array[Double]] = None
  private var fittedData: Option[Data] = None
  
  def getEquation: Option[Array[Double]] = equation
  def getResiduals: Option[Array[Double]] = residuals
  def getFittedData: Option[Data] = fittedData

  def fitData: Unit = 
  {
    // listwise deletion for missing values
    if (fittedData.isEmpty) checkAndDeleteMissingRows
    // check that there are enough rows to fit
    
    // calculate the normal equation
    
    // calculate the residuals
  }
  
  def checkAndDeleteMissingRows = 
  {
    
  }
  
}