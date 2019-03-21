package regressionViz

abstract class DataReader 
{
  def readFile(filename: String): Option[Data]
}