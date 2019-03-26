package regressionViz

import scala.collection.mutable.ArrayBuffer

class CSVReader extends DataReader 
{
  // example here: https://alvinalexander.com/scala/csv-file-how-to-process-open-read-parse-in-scala
  def readFile(filename: String): Option[Data] =
  {
    // create array to be filled
    // needs to be as long as the file has columns
    // TODO: does the file include variable names?
    var dataBuffer: ArrayBuffer[ArrayBuffer[Double]] = 
      ArrayBuffer[ArrayBuffer[Double]]()
    
    val bufferedFile = io.Source.fromFile(filename)  
    var firstLine = true
    try
    {
      for (line <- bufferedFile.getLines())
      {
        val row = line.split(",").map(_.trim)
        // TODO: fill the array
        if(firstLine)
        {
          for(index <- 0 until row.length)
            dataBuffer += ArrayBuffer[Double]()
          firstLine = false
        }
        
        
      }
    } 
    finally
    {
      bufferedFile.close()
    }
    
    
    
    None
  }
}