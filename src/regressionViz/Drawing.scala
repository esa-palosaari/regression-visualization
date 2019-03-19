package regressionViz

// trying things out for now
// using this example: http://otfried.org/scala/drawing.html
import java.awt.image.BufferedImage
import java.awt.{Graphics2D, Color, Font, BasicStroke}
import java.awt.geom._
import scala.math._

class Drawing (val model: Model) 
{
  require(model.getFittedData.isDefined &&
          model.getEquation.isDefined)
  // image size
  val size = (500, 500)
  
  // margin
  val margin = 0.10
  
  // canvas to draw on 
  val canvas = new BufferedImage(size._1, size._2,
                                 BufferedImage.TYPE_INT_RGB)
  
  // 2D graphics
  val g = canvas.createGraphics()
  
  // background
  g.setColor(Color.WHITE)
  g.fillRect(0,0, canvas.getWidth, canvas.getHeight)
  
  // anti-aliased rendering
  g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, 
                     java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
  
                                         
  // create coordinate axes
  val points = model.getFittedData.get.getPoints.get
  val y = points.last
  val x = points(0)
  val maxX = x.max 
  val minX = x.min
  val maxY = max(y.max, 
                 max(model.getEquation.get(0), 
                     max(model.getEquation.get(0)+model.getEquation.get(1)*maxX,
                         model.getEquation.get(0)+model.getEquation.get(1)*minX
                         )
                     )
                )
  val minY = min(y.min, 
                 min(model.getEquation.get(0), 
                     min(model.getEquation.get(0)+model.getEquation.get(1)*maxX,
                         model.getEquation.get(0)+model.getEquation.get(1)*minX
                         )
                     )
                )
  var xHeight = size._2*(1.0- margin)
  var yWidth = size._1*margin
  
  var axisXUnit: Int = ((size._1.toDouble*(1.0-(margin*2.0)))/
                         max(abs(maxX-minX),abs(minX-maxX))).toInt
    
  var axisYUnit: Int = (size._2.toDouble*(1.0-(margin*2))/
                        max(abs(maxY-minY), abs(minY-maxY))).floor.toInt


  // draw coordinate lines
  g.setStroke(new BasicStroke())
  g.setColor(new Color(0,0,0)) // set to black
  g.draw(new Line2D.Double(
                             margin*size._1, 
                             xHeight, 
                             (1.0-margin)*size._1, 
                             xHeight
                          )
        )
  g.draw(new Line2D.Double(
                              yWidth, 
                              margin*size._2, 
                              yWidth, 
                              (1.0-margin)*size._2
                           )
         )
  
  // draw data points
  g.setColor(Color.RED)
  (points(0) zip points.last).map(
                                    x => g.fill(
                                                 new Ellipse2D.Double(
                                                   (size._1*margin) + (x._1-minX)*axisXUnit, 
                                                   (size._2*(1.0-margin)) - (x._2-minY)*axisYUnit,
                                                   5.0, // size
                                                   5.0 // size   
                                                 )
                                               ) 
                                   )
                                   
  // draw the regression line
  g.setColor(Color.BLUE)
  g.draw(new Line2D.Double(
                             margin*size._1,
                             size._2*(1.0-margin) - axisXUnit*(model.getEquation.get(0)-minY),
                             (1.0-margin)*size._1,
                             size._2*(1.0-margin) - axisXUnit*(model.getEquation.get(0)+
                                                         model.getEquation.get(1)*maxX-minY))
  )
  
  g.dispose()
  
  javax.imageio.ImageIO.write(canvas, 
                              "png", 
                              new java.io.File("drawing.png"))
  
}