package regressionViz

import scala.collection.mutable.ArrayBuffer
import java.io._
import scala.xml._

/*
 * https://dzone.com/articles/working-with-xml-in-scala
 */

class XMLReader (var1Name: String, var2Name: String) extends DataReader
{
  def readFile(filename: String): Option[Data] =
  {
    try
    {
      val xmlFile = XML.load(filename)
      
    }
    
    None
  }
}