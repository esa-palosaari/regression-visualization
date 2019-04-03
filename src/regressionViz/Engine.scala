package regressionViz

import scala.collection.mutable.ArrayBuffer

class Engine {
  
  var data: ArrayBuffer[Data] = ArrayBuffer[Data]()
  var models: ArrayBuffer[Model] = ArrayBuffer[Model]()
  var visuals: ArrayBuffer[Drawing] = ArrayBuffer[Drawing]()
  
  def readData(fileName: String) 
  {
    if (fileName.toLowerCase.endsWith(".csv"))
    {
      val reader = new CSVReader
      data += reader.readFile(fileName).get
    }
    
  }
  
  def fitModel() 
  {
    
  }
  
  def saveImage(image: Drawing, fileName: String) 
  {
    
  }
}