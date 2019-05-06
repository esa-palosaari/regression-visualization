package regressionViz

import scala.collection.mutable.ArrayBuffer
import java.io._

class CSVReader(var1Name: String, var2Name: String) extends DataReader 
{
  // example here: https://alvinalexander.com/scala/csv-file-how-to-process-open-read-parse-in-scala
  // https://alvinalexander.com/scala/scala-exception-handling-try-catch-finally
  def readFile(filename: String): Option[Data] =
  {
    if(filename=="") return None
    // create array to be filled
    // needs to be as long as the file has columns

    var dataBuffer: ArrayBuffer[ArrayBuffer[Double]] = 
      ArrayBuffer[ArrayBuffer[Double]]()

    // example: https://alvinalexander.com/scala/scala-exception-handling-try-catch-finally      
    val bufferedFile = { 
      try 
      {
        io.Source.fromFile(filename)  
      }
      catch
      {
        case e: FileNotFoundException => throw e
        case e: Exception => 
          {
            val readExcept = new Exception(
                "Problem reading the file " + filename)
            throw readExcept
          }
            
        
      }
    }
    var firstLine = true
    try
    {
      var lineNumber = 0
      for (line <- bufferedFile.getLines)
      {
        val row = line.split(",").map(_.trim)
        // TODO: handle missing values
        // fill the array
        if(firstLine == true)
        {
          for(index <- 0 until row.length)
            dataBuffer += ArrayBuffer[Double]()
          firstLine = false
        }
        
        for(index <- 0 until row.length)
        {
          dataBuffer(index) += row(index).toDouble
        }
        
        lineNumber += 1
      }
    } 
    catch
    {
      case e:FileNotFoundException => throw e
      case e:Exception =>
        {
          val csvReaderException = new Exception(
              "Reading a csv file failed.")
          csvReaderException.initCause(e)
          throw csvReaderException          
        }
    }
    finally
    {
      bufferedFile.close()
    }
    try
    {
      var dataToArrays = dataBuffer.map(_.toArray).toArray
      val data = new Data
      var varNames: Option[ArrayBuffer[String]] = Some(ArrayBuffer(var1Name, var2Name))
      if (var1Name.equals("") || var2Name.equals("")) varNames = None
      data.initializeDataset(newPoints = Some(dataToArrays),
                             newVarNames=varNames)
      Some(data)  
    }
    catch
    {
      case e:Exception =>
        {
          val csvReaderException = new Exception(
              "Initializing data from a csv file failed.")
          csvReaderException.initCause(e)
          throw csvReaderException          
        }
    }    
  }
}