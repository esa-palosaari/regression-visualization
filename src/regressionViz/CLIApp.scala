package regressionViz

// example to follow: https://stackoverflow.com/questions/2315912/best-way-to-parse-command-line-parameters
object CLIApp {
  val usage = """
      Options: cliapp data.csv output.png
    """
  //val engine = new Engine
  
  def main(args: Array[String]) 
  {
    if(args.length == 0) println(usage)
    val argumentList = args.toList
    type OptionMap = Map[Symbol, Any]
    
    def nextOption(map: OptionMap, list: List[String]): OptionMap =
    {
      def isSwitch(s: String) = (s(0) == '-')
      list match 
      {
        case Nil => map
        case string :: tail => nextOption(map ++ Map('datafile -> string), list.tail)
        case string :: Nil => nextOption(map ++ Map('imagefile -> string), list.tail)
        case option :: tail => println("Unknown option " + option)
                              sys.exit(1)
      }
    }
    val options = nextOption(Map(), argumentList)
    println(options)
  }
}