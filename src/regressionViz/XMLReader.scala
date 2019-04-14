package regressionViz

import scala.collection.mutable.ArrayBuffer
import java.io._
import scala.xml._

/*
 * Using examples from https://dzone.com/articles/working-with-xml-in-scala
 */

class XMLReader (var1Name: String, var2Name: String) extends DataReader
{
  def readFile(filename: String): Option[Data] =
  {
    if( filename.equals("") || 
        var1Name.equals("") || 
        var2Name.equals("")) 
      return None
      
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
      val dataToArrays = dataBuffer.map(_.toArray).toArray
      
      if( dataBuffer(0).length == 0 || 
          dataBuffer(1).length == 0) 
        return None // return None if there was no data
        
      val data = new Data
      data.initializeDataset(newPoints = Some(dataToArrays), 
                             newVarNames = Some(ArrayBuffer(var1Name, var2Name)))
      return Some(data)
    }
    catch
    {
      case e: Exception =>
        {
          val xmlReaderException = new Exception(
              "Reading an xml file failed.")
          xmlReaderException.initCause(e)
          throw xmlReaderException
        }
    }
    
    None
  }
}