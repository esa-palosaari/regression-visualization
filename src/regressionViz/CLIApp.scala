package regressionViz

// examples followed: https://stackoverflow.com/questions/2315912/best-way-to-parse-command-line-parameters

/*
 * 	How to compile jar in Eclipse:
 * 
 *  How to run the jar file:
 *  Install scala 2.12.3
 *  Install java 8 or later
 *  scala -cp "${CLASSPATH}:${SCALA_HOME}/lib/scala-library.jar:regressionVizPack.jar" regressionViz.CLIApp data output   
 */
object CLIApp {
  val usage = """
      Usage: cliapp --d dataFileName.csv --o outputFileName.png 
                      --modeltype [normal | log]
                      --xmax [max_x-axis_value] --xmin [min_x-axis_value] 
                      --ymax [max_y-axis_value] --ymin [min_y-axis_value]
                      --pointcolor [COLORNAME] --curvecolor [COLORNAME]
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
    
    args.sliding(2, 2).toList.collect 
    {
      case Array("--modeltype", "normal") => modelType = "normal"
      case Array("--modeltype", "log") => modelType = "log"
      case Array("--d", dname: String) => dataFilename = dname
      case Array("--o", iname: String) => imageFilename = iname
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