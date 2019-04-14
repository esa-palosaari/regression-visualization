package regressionViz

import scala.collection.mutable.ArrayBuffer

class Engine {
  
  var data: ArrayBuffer[Data] = ArrayBuffer[Data]()
  var models: ArrayBuffer[Model] = ArrayBuffer[Model]()
  var visuals: ArrayBuffer[Drawing] = ArrayBuffer[Drawing]()
  

  def readData(fileName: Option[String], 
               var1Name: Option[String], 
               var2Name: Option[String]) =
  {
    try
    {
      fileName match 
      {
        case None => throw new Exception("No file name")
        case Some(fileName) =>
          {
            if (fileName.toLowerCase.endsWith(".xml"))
            {
              if (var1Name.isEmpty || var2Name.isEmpty) 
                throw new Exception("XML data requires variable names.") 
              val xmlReader = new XMLReader(var1Name.get, var2Name.get)
              val xmlData = xmlReader.readFile(fileName)
              if (xmlData.isEmpty)
                throw new Exception("Reading data from file " + fileName + " failed.")
              data += xmlData.get
            }
      
            if (fileName.toLowerCase.endsWith(".csv"))
            {
              val csvReader = new CSVReader(var1Name.getOrElse(""),
                                         var2Name.getOrElse("")
                                         )
              val csvData = csvReader.readFile(fileName)
              if(csvData.isEmpty)
                throw new Exception("Reading data from file " +fileName+ " failed.")
              data += csvData.get
            }            
           }
      }

    }
    catch
    {
      case e:Exception => 
        {
          val readerException = new Exception(
              "Reading data from file failed")
          readerException.initCause(e)
          throw readerException
        }
    }
    
  }
  
  def fitModel(modelType: String, dataToFit: Data) =
  {
    modelType match 
    {
      case "quad" => 
        {
          val quadModel = new QuadModel(dataToFit)
          quadModel.fitData
          models += quadModel
        }
      case _ => 
        {
          val unfitModel = new OLSModel(dataToFit)
          unfitModel.fitData
          models += unfitModel
        }
    }
  }
  
  def drawImage(  model: Model,
                  sizex: Option[Int],
                  sizey: Option[Int],
                  xmax: Option[Int],
                  xmin: Option[Int],
                  ymax: Option[Int],
                  ymin: Option[Int],
                  pR: Option[Int],
                  pB: Option[Int],
                  pG: Option[Int],
                  cR: Option[Int],
                  cB: Option[Int],
                  cG: Option[Int]
                ) = 
  {
     visuals += new Drawing(  model, 
                              sizex,
                              sizey,
                              xmax,
                              xmin,
                              ymax,
                              ymin,
                              pR,
                              pB,
                              pG,
                              cR,
                              cB,
                              cG
                           )
  }
  
  def saveImage(image: Drawing, fileName: String) =
  {
    javax.imageio.ImageIO.write(  image.canvas, 
                                  "png", 
                                  new java.io.File(fileName)
                               )
  }
}