package regressionViz

import scala.math._

class QuadModel (data: Data) extends Model(data) {

  // get matrix functions
  val matrix = new ArrayMatrix
  
  def getY(x: Double): Double =
  {
    require(equation.isDefined)
    equation.get(0) + equation.get(1)*x + equation.get(2)*pow(x,2)
  }   
  

   def equationToString: String =
   {
     require(equation.isDefined)
     val coef = equation.get.map(x => roundDouble(x, 2))
     val nameY = getFittedData.get.getVarNames.get(1)
     val nameX = getFittedData.get.getVarNames.get(0)
     var equationString = nameY+" = "+ coef(0) +" + ("+ coef(1) +")"+nameX+ 
                          " + ("+coef(2)+")"+nameX+"^2"
     return equationString
   }  
  
  // TODO: a maximum and minimum Y for non-linear models?
  def fitData: Unit = 
  {
    // TODO: listwise deletion for missing values needs updating?
    if (fittedData.isEmpty) 
    {
      try checkAndDeleteMissingRows
      catch
      {
        case e:Exception =>
        {
          val missingException = new Exception(
              "Problems in checking and deleting missing rows.")
          missingException.initCause(e)
          throw missingException
        }
      }
    }
    
    // check that there are enough rows and columns
    // after deleting rows with missing values
    if(!(fittedData.isDefined && 
        fittedData.get.getPoints.isDefined &&
        fittedData.get.getPoints.get.length > 1 &&
        fittedData.get.getPoints.get(0).length> 1))
    {
      val fitDataException = new Exception(
          "There should be enough rows and columns " +
          "in the dataset after listwise deletion.")
      throw fitDataException
    }
    try
    {
      calculateNormalEquation
      calculateResiduals  
    }
    catch
    {
      case e:Exception =>
        {
          val normalException = new Exception(
              "Problems in estimating the normal equation " +
              "and the residuals.")
          normalException.initCause(e)
          throw normalException  
        }
    }
    
  }
  
  protected def calculateNormalEquation =
  {
    // add the intercept to the equation (i.e. column of ones)
    // take the last column out = outcome variable
    val X: Array[Array[Double]] =
    {  
      val originalPoints = fittedData.get.getPoints.get
      val originalLength = originalPoints(0).length
      val oneVector = Array.fill(originalLength)(1.0)
      val oneIncluded = oneVector +: originalPoints.dropRight(1)
      val toSecondPower = Array.fill(originalLength)(0.0)
      originalPoints(0).zipWithIndex.map(x => toSecondPower(x._2) = pow(x._1,2))
      val secondIncluded = oneIncluded :+ toSecondPower
      secondIncluded
    }
    // outcome variable is the last one
    val y: Array[Double] = fittedData.get.getPoints.get.last
    val Xt: Array[Array[Double]] = X.transpose
    val XtX: Array[Array[Double]] = matrix.multiplyMatrices(Xt, X)
    val Xty: Array[Double] = matrix.multiplyMatrixAndVector(Xt, y)
    val XtXInverse: Array[Array[Double]] = matrix.invertMatrix(XtX)
    val estimate: Array[Double] = matrix.multiplyMatrixAndVector(XtXInverse, Xty)

    equation = Some(estimate)
  }
  
  def calculateResiduals =
  {
    require(equation.isDefined && 
            fittedData.isDefined &&
            fittedData.get.getPoints.isDefined)
            
    val N = fittedData.get.getPoints.get(0).length  
    val fittedPoints = fittedData.get.getPoints.get
    var calculatedResiduals: Array[Double] = Array.ofDim(N)
                                                         
    for (i <- 0 until N)
    {
      calculatedResiduals(i) = fittedPoints.last(i) - (equation.get(0) + 
          equation.get.tail.zip(fittedPoints).map(x => x._1 * x._2(i)).reduce(_ + _)) 
    }
     
    residuals = Some(calculatedResiduals)
  }  
}