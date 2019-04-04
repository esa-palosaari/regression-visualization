package regressionViz

import scala.collection.mutable.ArrayBuffer

class Engine {
  
  var data: ArrayBuffer[Data] = ArrayBuffer[Data]()
  var models: ArrayBuffer[Model] = ArrayBuffer[Model]()
  var visuals: ArrayBuffer[Drawing] = ArrayBuffer[Drawing]()
  
  def readData(fileName: String) =
  {
    if (fileName.toLowerCase.endsWith(".csv"))
    {
      val reader = new CSVReader
      data += reader.readFile(fileName).get
    }
    
  }
  
  def fitModel(modelType: String, dataToFit: Data) =
  {
    modelType match 
    {
      case _ => models += new OLSModel(dataToFit)
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