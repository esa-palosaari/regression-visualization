package regressionViz

abstract class Model (data: Data) 
{
  //require(data.getPoints.isDefined, "The data is empty. Model creation failed.")
  
  private var name: Option[String] = Some("jotain")
  private var equation: Option[Array[Double]] = Some(Array(1, 2))
  private var residuals: Option[Array[Double]] = Some(Array(1,2))
  
  def fitData(data: Data): Unit
  
  def getName: Option[String] = Some("eri")
  
  
}