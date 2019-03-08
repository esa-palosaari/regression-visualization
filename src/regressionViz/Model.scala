package regressionViz

abstract class Model (data: Data) 
{
  require(data.getPoints.isDefined && data.getPoints.get(0).length > 0
      , "The data is empty. Model creation failed.")
  
  var name: Option[String] = None
  protected var equation: Option[Array[Double]] = None
  protected var residuals: Option[Array[Double]] = None
  protected var fittedData: Option[Data] = None
  
  def getEquation: Option[Array[Double]] = equation
  def getResiduals: Option[Array[Double]] = residuals
  def getFittedData: Option[Data] = fittedData  
  
  def fitData: Unit
  
  def checkAndDeleteMissingRows = 
  {
    // get the original array
    
    // go through all rows
    
    // check whether there is a missing value
    
    // if there are not missing values, add the row to new dataset
    
    // add fittedData
  }
  
}