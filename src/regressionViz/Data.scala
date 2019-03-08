package regressionViz

import scala.collection.mutable.ArrayBuffer

class Data() 
{

  private var points: Option[Array[Array[Double]]] = None
  var name: Option[String] = None
  private var varNames: Option[ArrayBuffer[String]] = None
  
  def getPoints: Option[Array[Array[Double]]] = points
  def getVarNames: Option[ArrayBuffer[String]] = varNames
  
  /*
   *  InitializeDataset allows to replace all, some or none of the 
   *  default. Needed for checking and creating variable names.
   */
  def initializeDataset (newPoints: Option[Array[Array[Double]]] = None,
                         newName: Option[String] = None,
                         newVarNames: Option[ArrayBuffer[String]] = None): Unit = 
  {
    newPoints match 
    {
      case Some(datapoints) => 
        {
          if(!datapoints.forall(_.length == datapoints(0).length)) 
            throw new IllegalArgumentException("Unequal number of data in columns.")
          points = newPoints
        }
      case None => 
    }
    
    newName match 
    {
      case Some(anotherName) => name = Some(anotherName)
      case None => 
    }
    
    newVarNames match 
    {
      case Some(newVarNames) => 
        {
          val varNamesLength = newVarNames.length
          if (points.isDefined && varNamesLength == points.get.length) 
            varNames = Some(newVarNames)
          else if (points.isDefined && varNamesLength < points.get.length)
          {
            addVariableNames(varNamesLength, points.get.length, false)
          }
        }
      case None => 
        {
          if(points.isDefined) addVariableNames(1, points.get.length, true)
        }  
    }
    
    // if there are no variable names, add them here
    def addVariableNames(from: Int, until: Int, createNew: Boolean) = 
    {
      var i = from
      if (createNew) varNames = Some(ArrayBuffer.fill(until)(""))
      else varNames = Some(varNames.get.++=(ArrayBuffer.fill(until-from)("")))
      while (i <= until)
      {
        varNames.get(i-1) = "Variable_" + i.toString()
        i += 1
      }
    }
  }
  
}