// examples followed: https://stackoverflow.com/questions/2315912/best-way-to-parse-command-line-parameters

/*
 * 	How to build a JAR in Eclipse:
 *  0. Have Scala 2.12 installed
 *  1. Check that the file scala-xml_2.12-1.0.5.jar in on Build Path
 *  2. Right click regressionViz in Project Explorer
 *  3. Click "Export"
 *  4. Choose Java folder
 *  5. Choose JAR file and click Next
 *  6. Choose name an location for the JAR file
 *  7. Click Finish
 * 
 *  How to excute the JAR file:
 *  1. Install Scala 2.12.3
 *  2. Install java 8 or later
 *  3. In shell enter command: scala -cp "${CLASSPATH}:${SCALA_HOME}/lib/scala-library.jar:NameOfTheJARExported.jar" regressionViz.CLIApp [options]   
 */
package regressionViz
import java.awt.Color

object CLIApp {
  val usage = """
      This app creats a visualisation of a regression model fitted to data.
      Data and output file names are required (and variable names if you are
      using xml files: they must match), the rest are optional and have default values.
      
      Usage: cliapp --d [path\to\dataFileName.csv|.xml] 
                    --o [path\to\outputFileName.png]
                    --varx [nameForXVariable]
                    --vary [nameForYVariable]
                    --modeltype [normal | quad]
                    --sizex [image size, horizontal] 
                    --sizey [image size, vertical]
                    --xmax [max_x-axis_value] 
                    --xmin [min_x-axis_value] 
                    --ymax [max_y-axis_value] 
                    --ymin [min_y-axis_value]
                    --pR [data point Red: 0-255] 
                    --pB [data point Blue: 0-255]  
                    --pG [data point Green: 0-255] 
                    --cR [regression curve Red: 0-255]
                    --cB [regression curve Blue: 0-255] 
                    --cG [regression curve Green: 0-255]
    """
  val engine = new Engine
  type OptionMap = Map[Symbol, Any]
  
  def main(args: Array[String]): Unit =
  {
    if(args.length == 0) println(usage)
    val argumentList = args.toList
    
    var dataFilename = ""
    var imageFilename = ""
    var modelType = ""
    var varx = ""
    var vary = ""
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
    try
    {
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
        case Array("--modeltype", "quad") => modelType = "quad"
        case Array("--xmax", xmaxValue: String) => xmax = Some(xmaxValue.toInt)
        case Array("--xmin", xminValue: String) => xmin = Some(xminValue.toInt)
        case Array("--ymax", ymaxValue: String) => ymax = Some(ymaxValue.toInt)
        case Array("--ymin", yminValue: String) => ymin = Some(yminValue.toInt)
        case Array("--varx", varXName: String) => varx = varXName
        case Array("--vary", varYName: String) => vary = varYName
        case Array("--d", dname: String) => dataFilename = dname
        case Array("--o", iname: String) => imageFilename = iname
        case _ => println("Could not parse all options.\n" + usage)
      }
    }
    catch
    {
      case e:Exception =>
        {
          println("Problem reading the options")
          println(e.getMessage)
          System.exit(1)                  
        }

    }
    /*
     * Käyttäjä pystyy lataamaan datajoukon valitsemastaan 
     * tiedostosta, joka voi sisältää suurenkin määrän 
     * (x, y)-koordinaattipareja.
     */
    try
    {
      if(!dataFilename.equals("")) 
      {
        if (varx.equals("") || vary.equals(""))
          engine.readData(Some(dataFilename), None, None)
        else
          engine.readData(Some(dataFilename), Some(varx), Some(vary))
      } 
      else 
      {
        println("Please give the name of the data file.")
        System.exit(1)
      }
    }
    catch
    {
      case e:Exception =>
        println(e.getMessage)
        System.exit(1)
    }

    
    /* 
     * Fits two types of OLS models: linear and quadratic
     */
    try
    {
      engine.fitModel(modelType, engine.data(0)) 
    }
    catch
    {
      case e: Exception =>
        println("Problem when fitting a model")
        println(e.getMessage)
        System.exit(1)
    }
     
      
    /*
     * Ohjelman tulee tehdä kuvaaja alkuperäisistä datapisteistä 
     * sekä niihin sovitetusta mallista. Käyttäjä pystyy 
     * säätämään kuvaajan asetuksia, esimerkiksi akselien 
     * päätepisteitä.
     */
    try
    {
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
    }
    catch
    {
      case e:Exception =>
        {
        	println("Problem when drawing an image")
        	println(e.getMessage)
        	System.exit(1)          
        }
    }       
    
    try
    {
    	if(imageFilename.equals(""))
    		engine.saveImage(engine.visuals(0), "image.png")
       else
  			engine.saveImage(engine.visuals(0), imageFilename)      
    }
        catch
    {
      case e:Exception =>
        {
        	println("Problem when saving an image")
        	println(e.getMessage)
        	System.exit(1)          
        }
    }   
      

     
  }
}