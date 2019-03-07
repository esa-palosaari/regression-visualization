package regressionViz

import scala.collection.mutable.ArrayBuffer

class Data() 
{

  private var points: Option[Array[Array[Double]]] = None
  private var name: Option[String] = None
  private var varNames: Option[ArrayBuffer[String]] = None
  
  def getPoints: Option[Array[Array[Double]]] = points
  def getName: Option[String] = name
  def getVarNames: Option[ArrayBuffer[String]] = varNames
  
  /*
   *  initializeDataset allows to replace all, some or none of the 
   *  default 
   */
  def initializeDataset (newPoints: Option[Array[Array[Double]]] = None,
                         newName: Option[String] = None,
                         newVarNames: Option[ArrayBuffer[String]] = None): Unit = 
  {
    newPoints match 
    {
      case Some(datapoints) => points = newPoints
      case None => 
    }
    
    newName match 
    {
      case Some(anotherName) => name = Some(anotherName)
      case None => 
    }
    
    newVarNames match 
    {
      case Some(newVarNames) => varNames = Some(newVarNames)
      case None => 
        {
          if(points.isDefined) 
          {
            var i = 1
            val columnNumber = points.get.length
            varNames = Some(ArrayBuffer.fill(columnNumber)(""))
            while (i <= columnNumber)
            {
              varNames.get(i-1) = "Variable_" + i.toString()
              i = i + 1
            }
          }
        }  
    }
  }
  
}