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
    

    // button for reading data
    val dataLoadButton = new Button
    {
      text = "Read the data file"
      foreground = Color.blue
      background = Color.white
      borderPainted = true 
      enabled = true
    }

    // input box for data filename
    val dataField = new TextField("Data file name")

    // input box for variable names    
    val firstVarField = new TextField("First variable")
    val secondVarField = new TextField("Second variable")
    
    // input box for image filename    
    
    // Panel for controls
    val gridPanel = new GridPanel(4,1)
    {
      contents += dataField
      contents += firstVarField
      contents += secondVarField
      contents += dataLoadButton
    }
    
    contents = new BorderPanel
    {
      layout(gridPanel) = East
      layout(messagePanel) = South
    }
    
    val dim = new Dimension(800, 800)
    minimumSize = dim
    preferredSize = dim
    foreground = Color.WHITE
    
    listenTo(dataLoadButton)
    
    reactions += 
    {
      case ButtonClicked(component) if component == dataLoadButton =>
        try
        {
          val filename = dataField.text
          if(!filename.equals("")) 
          {
            if (firstVarField.text.equals("") || secondVarField.text.equals(""))
            {
              engine.readData(Some(filename), None, None)
              messagePanel.text_=("" + filename + " read sucessfully")
              messagePanel.background = Color.green              
            }
            else
            {
              try
              {
                engine.readData(Some(filename), Some(firstVarField.text), Some(secondVarField.text))
                messagePanel.text_=("" + filename + " read sucessfully")
                messagePanel.background = Color.green
              }
              catch
              {
                case e: Exception =>
                {
                  messagePanel.text_=(e.getMessage)
                  messagePanel.background = Color.red
                }                
              }
            }

          } 
          else 
          {
            messagePanel.text_=("Please give the name of the data file.")
            messagePanel.background = Color.red
          }          
        }
        catch
        {
          case e: Exception =>
          {
            messagePanel.text_=(e.getMessage)
            messagePanel.background = Color.red
          }
        }
    }
  }
  

  

  
  // button for writing an image
  
  // choose model
  
  // window for the image
  
  // image size
  
  // vertical axis size
  
  // horizontal axis size
  
  // color values for data points
  
  // color values for the regression curve
  

  
}