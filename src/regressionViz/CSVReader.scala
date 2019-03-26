package regressionViz

import scala.collection.mutable.ArrayBuffer

class CSVReader extends DataReader 
{
  // example here: https://alvinalexander.com/scala/csv-file-how-to-process-open-read-parse-in-scala
  def readFile(filename: String): Option[Data] =
  {
    if(filename=="") return None
    // create array to be filled
    // needs to be as long as the file has columns
    // TODO: does the file include variable names?
    var dataBuffer: ArrayBuffer[ArrayBuffer[Double]] = 
      ArrayBuffer[ArrayBuffer[Double]]()
    
    val bufferedFile = io.Source.fromFile(filename)  
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
    finally
    {
      bufferedFile.close()
    }
     
    var dataToArrays = dataBuffer.map(_.toArray).toArray
    val data = new Data
    data.initializeDataset(newPoints = Some(dataToArrays))
    Some(data)
  }
}