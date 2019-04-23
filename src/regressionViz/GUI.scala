package regressionViz

import scala.swing._
import java.awt.Color

object GUI extends SimpleSwingApplication {
  
  val engine = new Engine
  
  // the main window
  def top = new MainFrame
  {
    title = "Regression visualization"
    contents = new BoxPanel(Orientation.Horizontal)
    size = new Dimension(700, 700)
  }
  
  // message window
  
  // input box for data filename
  
  // input box for image filename
  
  // button for reading data
  
  // button for writing an image
  
  // choose model
  
  // window for the image
  
  // image size
  
  // vertical axis size
  
  // horizontal axis size
  
  // color values for data points
  
  // color values for the regression curve
  
  // input box for variable names
  
}