package regressionViz

// example to follow: https://stackoverflow.com/questions/2315912/best-way-to-parse-command-line-parameters

/*
 *  How to run the jar file:
 *  Install scala 2.12.3
 *  Install java 8 or later
 *  scala -cp "${CLASSPATH}:${SCALA_HOME}/lib/scala-library.jar:regressionVizPack.jar" regressionViz.CLIApp data output   
 */
object CLIApp {
  val usage = """
      Options: cliapp --d data.csv --o output.png
    """
  val engine = new Engine
  type OptionMap = Map[Symbol, Any]
  
  def main(args: Array[String]): (String, String) =
  {
    if(args.length == 0) println(usage)
    val argumentList = args.toList
    
    var dataFilename = ""
    var imageFilename = ""
    
    args.sliding(2, 2).toList.collect 
    {
      case Array("--d", dname: String) => dataFilename = dname
      case Array("--o", iname: String) => imageFilename = iname
    }
    
    if(!dataFilename.equals("")) 
    {
      engine.readData(dataFilename)  
    } else println("Please give the name of the data file.")
    
    
    (dataFilename, imageFilename)

  }
}