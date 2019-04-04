// examples followed: https://stackoverflow.com/questions/2315912/best-way-to-parse-command-line-parameters

/*
 * 	How to compile jar in Eclipse:
 * 
 *  How to run the jar file:
 *  Install scala 2.12.3
 *  Install java 8 or later
 *  scala -cp "${CLASSPATH}:${SCALA_HOME}/lib/scala-library.jar:regressionVizPack.jar" regressionViz.CLIApp [options]   
 */
package regressionViz
import java.awt.Color

object CLIApp {
  val usage = """
      This app creats a visualisation of a regression model fitted to data.
      Data and output file names are required, the rest are optional.
      
      Usage: cliapp --d [path\to\dataFileName.csv|.xml] --o [path\to\outputFileName.png] 
                      --modeltype [normal | log]
                      --sizex [image size, horizontal] --sizey [image size, vertical]
                      --xmax [max_x-axis_value] --xmin [min_x-axis_value] 
                      --ymax [max_y-axis_value] --ymin [min_y-axis_value]
                      --pR [data point Red: 0-255] --pB [data point Blue: 0-255]  
                      --pG [data point Green: 0-255] --cR [regression curve Red: 0-255]
                      --cB [regression curve Blue: 0-255] --cG [regression curve Green: 0-255]
    """
  val engine = new Engine
  type OptionMap = Map[Symbol, Any]
  
  def main(args: Array[String]): (String, String) =
  {
    if(args.length == 0) println(usage)
    val argumentList = args.toList
    
    var dataFilename = ""
    var imageFilename = ""
    var modelType = ""
    var sizex: Option[Int] = None
    var sizey: Option[Int] = None
    var xmax: Option[Int] = None
    var xmin: Option[Int] = None
    var ymax: Option[Int] = None
    var ymin: Option[Int] = None
    var pointColorR: Option[Int] = None
    var pointColorB: Option[Int] = None
    var pointColorG: Option[Int] = None
    var curveColorR: Option[Int] = None
    var curveColorB: Option[Int] = None
    var curveColorG: Option[Int] = None
    
    /*
     * Parse command line options
     */
    args.sliding(2, 2).toList.collect 
    {
      case Array("--sizex", imageX: String) => sizex = Some(imageX.toInt)
      case Array("--sizey", imageY: String) => sizey = Some(imageY.toInt)
      case Array("--pR", pointR: String) => 
        {
          require(pointR.toInt < 256 && pointR.toInt >= 0, 
                  "RBG values are 0-255")
          pointColorR = Some(pointR.toInt) 
        }
      case Array("--pB", pointB: String) => 
        {
          require(pointB.toInt < 256 && pointB.toInt >= 0, 
                "RBG values are 0-255")
          pointColorB = Some(pointB.toInt) 
        }
      case Array("--pG", pointG: String) => 
        {
          require(pointG.toInt < 256 && pointG.toInt >= 0, 
                  "RBG values are 0-255")          
          pointColorG = Some(pointG.toInt) 
        }
      case Array("--cR", curveR: String) => 
        {
          require(curveR.toInt < 256 && curveR.toInt >= 0, 
                  "RBG values are 0-255")          
          curveColorR = Some(curveR.toInt) 
        }
      case Array("--cB", curveB: String) => 
        {
          require(curveB.toInt < 256 && curveB.toInt >= 0, 
                  "RBG values are 0-255")
          curveColorB = Some(curveB.toInt) 
        }
      case Array("--cG", curveG: String) => 
        {
          require(curveG.toInt < 256 && curveG.toInt >= 0, 
                  "RBG values are 0-255")
          curveColorG = Some(curveG.toInt) 
        }
      case Array("--modeltype", "normal") => modelType = "normal"
      case Array("--modeltype", "log") => modelType = "log"
      case Array("--xmax", xmaxValue: String) => xmax = Some(xmaxValue.toInt)
      case Array("--xmin", xminValue: String) => xmin = Some(xminValue.toInt)
      case Array("--ymax", ymaxValue: String) => ymax = Some(ymaxValue.toInt)
      case Array("--ymin", yminValue: String) => ymin = Some(yminValue.toInt)
      case Array("--d", dname: String) => dataFilename = dname
      case Array("--o", iname: String) => imageFilename = iname
      case _ => println("Could not parse all options.\n" + usage)
    }
    
    /*
     * Käyttäjä pystyy lataamaan datajoukon valitsemastaan 
     * tiedostosta, joka voi sisältää suurenkin määrän 
     * (x, y)-koordinaattipareja.
     * 
     * TODO: Tuki vähintään kahdelle erilaiselle tiedostomuodolle.
     */
    
    if(!dataFilename.equals("")) 
    {
      engine.readData(dataFilename)  
    } 
    else 
    {
      println("Please give the name of the data file.")
      System.exit(1)
    }
    
    /* 
     * TODO: Toteuta vähintään yksi regressiomenetelmä yksinkertaisen 
     * lineaariregression lisäksi. (Joko molemmat yhtälöt, tai 
     * ensimmäinen yhtälö sekä itse valitsemasi muu menetelmä)
     */
     engine.fitModel(modelType, engine.data(0))
      
    /*
     * Ohjelman tulee tehdä kuvaaja alkuperäisistä datapisteistä 
     * sekä niihin sovitetusta mallista. Käyttäjä pystyy 
     * säätämään kuvaajan asetuksia, esimerkiksi akselien 
     * päätepisteitä.
     */
     engine.drawImage(  engine.models(0),
                        sizex,
                        sizey,
                        xmax,
                        xmin,
                        ymax,
                        ymin,
                        pointColorR,
                        pointColorB,
                        pointColorG,
                        curveColorR,
                        curveColorB,
                        curveColorG
                      )
     
    if(imageFilename.equals(""))
      engine.saveImage(engine.visuals(0), "image.png")
    else
      engine.saveImage(engine.visuals(0), imageFilename)
    
    (dataFilename, imageFilename)

  }
}