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
      
      Usage: cliapp --d [dataFileName.csv|.xml] --o [outputFileName.png] 
                      --modeltype [normal | log]
                      --xmax [max_x-axis_value] --xmin [min_x-axis_value] 
                      --ymax [max_y-axis_value] --ymin [min_y-axis_value]
                      --pointColorR [Red: 0-255] --pointColorB [Blue: 0-255]  
                      --pointColorG [Green: 0-255] --curveColorR [Red: 0-255]
                      --curveColorB [Blue: 0-255] --curveColorG [Green: 0-255]
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
    var xmax: Option[Double] = None
    var xmin: Option[Double] = None
    var ymax: Option[Double] = None
    var ymin: Option[Double] = None
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
      case Array("--pointColorR", pointR: String) => pointColorR = Some(pointR.toInt) 
      case Array("--pointColorB", pointB: String) => pointColorB = Some(pointB.toInt) 
      case Array("--pointColorG", pointG: String) => pointColorG = Some(pointG.toInt) 
      case Array("--curveColorR", curveR: String) => curveColorR = Some(curveR.toInt) 
      case Array("--curveColorB", curveB: String) => curveColorB = Some(curveB.toInt) 
      case Array("--curveColorG", curveG: String) => curveColorG = Some(curveG.toInt) 
      case Array("--modeltype", "normal") => modelType = "normal"
      case Array("--modeltype", "log") => modelType = "log"
      case Array("--xmax", xmaxValue: String) => xmax = Some(xmaxValue.toDouble)
      case Array("--xmin", xminValue: String) => xmin = Some(xminValue.toDouble)
      case Array("--ymax", ymaxValue: String) => ymax = Some(ymaxValue.toDouble)
      case Array("--ymin", yminValue: String) => ymin = Some(yminValue.toDouble)
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
    } else println("Please give the name of the data file.")
    
    /* 
     * TODO: Toteuta vähintään yksi regressiomenetelmä yksinkertaisen 
     * lineaariregression lisäksi. (Joko molemmat yhtälöt, tai 
     * ensimmäinen yhtälö sekä itse valitsemasi muu menetelmä)
     */
    
    /*
     * Ohjelman tulee tehdä kuvaaja alkuperäisistä datapisteistä 
     * sekä niihin sovitetusta mallista. Käyttäjä pystyy 
     * säätämään kuvaajan asetuksia, esimerkiksi akselien 
     * päätepisteitä.
     */
    
    (dataFilename, imageFilename)

  }
}