package regressionViz

import scala.collection.mutable.ArrayBuffer
import java.io._
import scala.xml._

/*
 * https://dzone.com/articles/working-with-xml-in-scala
 */

class XMLReader (var1Name: String, var2Name: String) extends DataReader
{
  def readFile(filename: String): Option[Data] =
  {
    if(filename.equals("")) return None
    var dataBuffer: ArrayBuffer[ArrayBuffer[Double]] = 
      ArrayBuffer[ArrayBuffer[Double]]()    
    try
    {
      val xmlFile = XML.load(filename)
      val varOne = xmlFile \\ var1Name
      val varTwo = xmlFile \\ var2Name
      val oneBuffer = ArrayBuffer[Double]()
      varOne.map(x => oneBuffer += x.text.toDouble)
      dataBuffer += oneBuffer
      val twoBuffer = ArrayBuffer[Double]()
      varTwo.map(x => twoBuffer += x.text.toDouble)
      dataBuffer += twoBuffer
      var dataToArrays = dataBuffer.map(_.toArray).toArray
      val data = new Data
      data.initializeDataset(newPoints = Some(dataToArrays), 
                            newVarNames = Some(ArrayBuffer(var1Name, var2Name)))
      return Some(data)
    }
    
    None
  }
}