package regressionViz

import scala.collection.mutable.ArrayBuffer

abstract class Model (data: Data) 
{
  require(data.getPoints.isDefined && 
          data.getPoints.get(0).length > 1 &&
          data.getPoints.get.length > 1, 
          "There is not enough data to fit a model. Model creation failed.")
  
  var name: Option[String] = None
  protected var equation: Option[Array[Double]] = None
  protected var residuals: Option[Array[Double]] = None
  protected var fittedData: Option[Data] = None
  
  def getEquation: Option[Array[Double]] = equation
  def getResiduals: Option[Array[Double]] = residuals
  def getFittedData: Option[Data] = fittedData  
  def getY(x: Double): Double
  def fitData: Unit
  
  def checkAndDeleteMissingRows = 
  {
    // get the original array
    val originalPoints = data.getPoints.get
    val originalColumns = originalPoints.length
    
    // create a mutable array to store complete rows
    var fitPoints: ArrayBuffer[ArrayBuffer[Double]] = 
      ArrayBuffer[ArrayBuffer[Double]]()
    for ( i <- 0 until originalColumns) 
    {
      fitPoints += ArrayBuffer[Double]()
    }
      
    // go through all rows
    var rowIndex = 0
    
    while (rowIndex < originalPoints(0).length)
    {
      var missingFound = false
      // go through all columns
      var columnIndex = 0
      while (columnIndex < originalColumns && missingFound == false)
      {
        // check whether there is a missing value
        if(originalPoints(columnIndex)(rowIndex) equals Double.NaN) 
          missingFound = true
        else 
          columnIndex += 1
      }

      // if there are were no missing values, add the row to new dataset
      if (missingFound == false)
      {
        columnIndex = 0
        while (columnIndex < originalColumns)
        {
          fitPoints(columnIndex) += originalPoints(columnIndex)(rowIndex)
          columnIndex += 1
        } 
      }
      
      // move to next row
      rowIndex += 1
    }    
    // add fittedData if there are more than two rows
    if (fitPoints(0).length > 1) 
    {
      var fitPointsToArrays = fitPoints.map(_.toArray).toArray
      val cleanData = new Data
      val oldVarNames = data.getVarNames
      
      cleanData.initializeDataset(
        Some(fitPointsToArrays), 
        Some(data.name.getOrElse("Dataset") + " with listwise deletion"), 
        oldVarNames
      )
        
      fittedData = Some(cleanData)      
    }
    else
    {
      fittedData = None
    }
  }
  
  
  
}