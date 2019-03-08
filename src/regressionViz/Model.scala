package regressionViz

abstract class Model (data: Data) 
{
  require(data.getPoints.isDefined && data.getPoints.get(0).length > 0
      , "The data is empty. Model creation failed.")
  
  var name: Option[String] = None
  
  def getEquation: Option[Array[Double]]
  def getResiduals: Option[Array[Double]]
  
  def fitData: Unit
  
}