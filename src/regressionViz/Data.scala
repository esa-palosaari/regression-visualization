package regressionViz

import scala.collection.mutable.ArrayBuffer

class Data() 
{

  private var points: Option[Array[Array[Double]]] = None
  private var name: Option[String] = None
  private var varNames: Option[ArrayBuffer[String]] = None
  
  def getPoints: Option[Array[Array[Double]]] = points
  def getName = name
  def getVarNames: Option[ArrayBuffer[String]] = varNames
  
  def initializeDataset (newPoints: Option[Array[Array[Double]]] = None,
                         newName: Option[String] = None,
                         newVarNames: Option[ArrayBuffer[String]] = None): Unit = 
  {
    newPoints match {
      case Some(datapoints) => points = newPoints
      case None => 
    }
  }
  
}