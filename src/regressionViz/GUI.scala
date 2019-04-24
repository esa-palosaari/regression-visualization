package regressionViz

import scala.swing._
import java.awt.Color
import scala.swing.BorderPanel.Position._
import event._

object GUI extends SimpleSwingApplication {
  
  val engine = new Engine

  


  // the main window
  // examples: https://stackoverflow.com/questions/27621939/type-mismatch-error-when-adding-label-to-scala-swing-panel
  // https://www.geeksforgeeks.org/java-swing-jpanel-examples/
  def top = new MainFrame
  {
    title = "Regression visualization"
    
    // error/success messages
    val messagePanel = new TextArea
    {
      text = "Program started."
      background = Color.green
    }
    
    // trying out a button
    val button = new Button
    {
      text = "Try me!"
      foreground = Color.blue
      background = Color.red
      borderPainted = true 
      enabled = true
    }
      
    contents = new BorderPanel
    {
      layout(button) = East
      layout(messagePanel) = South
    }
    
    val dim = new Dimension(700, 700)
    minimumSize = dim
    preferredSize = dim
    foreground = Color.WHITE
    
    listenTo(button)
    
    reactions += 
    {
      case ButtonClicked(component) if component == button =>
        messagePanel.text_=("Button press successful!")
        messagePanel.background = Color.cyan
    }
  }
  
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