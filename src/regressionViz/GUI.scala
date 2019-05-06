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
    var modelType = ""
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
    
    // choose model 
    val modelButtons = new ButtonGroup
    val lineModelButton = new RadioButton("First order OLS model")
    val quadModelButton = new RadioButton("Second order polynomial OLS model")
    modelButtons.buttons += lineModelButton
    modelButtons.buttons += quadModelButton
    
    class selectLinear extends Action("Linear model")
    {
      def apply() =
      {
        modelType = "linear"
      }
    }
    
    val selectLinearModel = new selectLinear
    lineModelButton.action_=(selectLinearModel)
    
    class selectQuadratic extends Action("Quadratic model")
    {
      def apply() =
      {
        modelType = "quad"
      }
    }
    
    val selectQuadraticModel = new selectQuadratic
    quadModelButton.action_=(selectQuadraticModel)    

    // button to fit a model (and draw the results)
    val modelFitButton = new Button
    {
      text = "Fit the model to the data"
      foreground = Color.blue
      background = Color.white
      borderPainted = true 
      enabled = true
    }    
    
    // input box for image filename    
    
    // Panel for controls
    val gridPanel = new GridPanel(6,1)
    {
      contents += dataField
      contents += firstVarField
      contents += secondVarField
      contents += dataLoadButton
      contents += lineModelButton
      contents += quadModelButton
      contents += modelFitButton
    }
    
    // window for the image
    class vizPanel extends Panel
    {
      override def paintComponent(g: Graphics2D) =
      {
        val n = engine.visuals.length
        if (n > 0) g.drawImage(engine.visuals(n).canvas, 0, 0, null)
      }
      
    }
    
    val viz = new vizPanel
    
    contents = new BorderPanel
    {
      layout(gridPanel) = East
      layout(messagePanel) = South
      layout(viz) = Center
    }
    
    val dim = new Dimension(800, 800)
    minimumSize = dim
    preferredSize = dim
    foreground = Color.WHITE
    
    listenTo(dataLoadButton)
    listenTo(modelFitButton)
    
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
                  throw e
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
      case ButtonClicked(component) if component == modelFitButton =>
        try
        {
          
          engine.fitModel(modelType, engine.data(0))
          messagePanel.text_=("" + modelType + " fitted sucessfully")
          messagePanel.background = Color.green      
          engine.drawImage(engine.models(0),
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
           messagePanel.text_=("Model drawn successfully")
           viz.repaint()
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
  
  
  
  // image size
  
  // vertical axis size
  
  // horizontal axis size
  
  // color values for data points
  
  // color values for the regression curve
  
}