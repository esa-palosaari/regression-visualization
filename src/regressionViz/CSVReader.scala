package regressionViz

class CSVReader extends DataReader 
{
  // exmple here: https://alvinalexander.com/scala/csv-file-how-to-process-open-read-parse-in-scala
  def readFile(filename: String): Option[Data] =
  {
    // TODO: create array to be filled
    // needs to be as long as the file has columns
    // does the file include variable names?
    
    val bufferedFile = io.Source.fromFile(filename)  
    try
    {
      for (line <- bufferedFile.getLines())
      {
        val row = line.split(",").map(_.trim)
        // TODO: fill the array
      }
    } 
    finally
    {
      bufferedFile.close()
    }
    
    
    
    None
  }
}