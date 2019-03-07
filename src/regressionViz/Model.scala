package regressionViz

abstract class Model (data: Data) 
{
  require(data.getPoints.isDefined && data.getPoints.get(0).length > 0
      , "The data is empty. Model creation failed.")
  
  var name: Option[String] = None
  private var equation: Option[Array[Double]] = None
  private var residuals: Option[Array[Double]] = None
  
  def getEquation = equation
  def getResiduals = residuals
  
  def fitData(data: Data): Unit
  
  
}