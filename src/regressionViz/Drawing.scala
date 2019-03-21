package regressionViz

// trying things out for now
// using this example: http://otfried.org/scala/drawing.html as the basis

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
//                 max(model.getEquation.get(0), 
                     max(model.getEquation.get(0)+model.getEquation.get(1)*maxX,
                         model.getEquation.get(0)+model.getEquation.get(1)*minX
                         )
//                     )
                )
  val minY = min(y.min, 
//                 min(model.getEquation.get(0), 
                     min(model.getEquation.get(0)+model.getEquation.get(1)*maxX,
                         model.getEquation.get(0)+model.getEquation.get(1)*minX
                         )
//                     )
                )
  val xHeight = size._2*(1.0- margin)
  val yWidth = size._1*margin
  
  val axisXUnit: Int = ((size._1.toDouble*(1.0-(margin*2.0)))/
                         max(abs(maxX.toInt-minX.toInt),abs(minX.toInt-maxX.toInt))).floor.toInt
    
  val axisYUnit: Int = (size._2.toDouble*(1.0-(margin*2.0))/
                        max(abs(maxY.toInt-minY.toInt), abs(minY.toInt-maxY.toInt))).floor.toInt
                        
  val axisUnit: Int = min(axisXUnit, axisYUnit)


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
         
  // draw x- and y-axes if visible
  g.setColor(Color.GRAY)
  // y-axis
  g.draw(new Line2D.Double(
                            margin*size._1 - axisUnit*minX,                        
                            margin*size._2,
                            margin*size._1 - axisUnit*minX,                        
                            (1.0-margin)*size._2
                           )
  )
  // x-axis
  g.draw(new Line2D.Double(
                             margin*size._1,
                             (1.0-margin)*size._2 + axisUnit*minY,
                             (1.0-margin)*size._1,
                             (1.0-margin)*size._2 + axisUnit*minY
                           )
  )
  
  // write numbers on x- and y-axes
  g.setFont(new Font("Arial", Font.PLAIN, 12))
  // zeros, TODO: update writing 0 to handle cases where the origo is outside the screen?
//  g.drawString(0.toString, (margin*size._1 - axisUnit*minX).toInt, ((1-0.7*margin)*size._2).toInt)
//  g.drawString(0.toString, (0.7*margin*size._1).toInt, ((1.0-0.7*margin)*size._2 + axisUnit*minY).toInt)

  // number of axisUnits that fit on the coordinate axes
  val numberOfUnitsX = ((size._1.toDouble*(1.0-(margin*2.0)))/axisUnit).floor.toInt
  val numberOfUnitsY = ((size._1.toDouble*(1.0-(margin*2.0)))/axisUnit).floor.toInt

  // get the order of the variable values
  var orderX = max(log10(abs(maxX)).floor.toInt, log10(abs(minX)).floor.toInt)
  var orderY = max(log10(abs(maxY)).floor.toInt, log10(abs(minY)).floor.toInt)
  // get the rounded end points 
  // TODO: check whether x-ticks and y-ticks are same order
  // TODO: Check again how to transform to different orders
  val smallestTickX = ((minX.ceil.toInt + pow(10,orderX)/2)/pow(10,orderX))*pow(10,orderX)
  val largestTickX = ((maxX.floor.toInt + pow(10,orderX)/2)/pow(10,orderX))*pow(10,orderX)
  val smallestTickY = ((minY.ceil.toInt + pow(10,orderY)/2)/pow(10,orderY))*pow(10,orderY)
  val largestTickY = ((maxY.ceil.toInt + pow(10,orderY)/2)/pow(10,orderY))*pow(10,orderY)

  // write the numbers from the smallest to the largest
  // how many ticks smaller is maX than the image boundary? TODO: check if extraTick works with negative maxX
  val extraTicksX = ((size._1.toDouble*(1.0-(2*margin)) - abs(maxX)*axisUnit).toInt/
                      axisUnit).toInt

  val extraTicksY = ((size._2.toDouble*(1.0-(2*margin)) - abs(maxY)*axisUnit).toInt/
                      axisUnit).toInt                      
  // x-axis
  var index = largestTickX.toInt + extraTicksX.toInt //+ pow(10, orderX))/2)/pow(10, orderX))*pow(10, orderX).toInt
  while (index >= minX)
  {
    g.drawString(  
                  index.toString, 
                  (margin*size._1  + (1+ index*pow(10,orderX))*axisUnit).toInt, // + abs(extraTicksX)*axisUnit
                  ((1-0.7*margin)*size._2).toInt
                )
    index -= pow(10,orderX).toInt
  }
  // y-axis  
  index = largestTickY.toInt + extraTicksY.toInt //+ pow(10, orderX))/2)/pow(10, orderX))*pow(10, orderX).toInt
  while (index >= minY)
  {
    g.drawString(  
                  index.toString, 
                  (0.7*margin*size._1).toInt, 
                  ((1.0-margin)*size._2  - (1+ index*pow(10,orderY))*axisUnit).toInt
                )
    index -= pow(10,orderY).toInt
  }  
  

  // get the order of the variable values
  
  // write the names of variables
  g.drawString(  model.getFittedData.get.getVarNames.get(0), 
                 size._1/2, 
                 ((1.0-0.3*margin)*size._2).toInt
              )
  // rotate string
  // https://stackoverflow.com/questions/10083913/how-to-rotate-text-with-graphics2d-in-java
  var transformation: AffineTransform = new AffineTransform()
  val plainFont: Font = new Font("Arial", Font.PLAIN, 12)
  transformation.rotate(toRadians(-90), 0, 0)
  val rotatedFont = plainFont.deriveFont(transformation)
  g.setFont(rotatedFont)
  g.drawString(
                model.getFittedData.get.getVarNames.get.last, 
                (0.3*margin*size._1).toInt ,
                size._2/2
              )
  // draw data points
  g.setColor(Color.RED)
  (points(0) zip points.last).map(
                                    x => g.fill(
                                                 new Ellipse2D.Double(
                                                   (size._1*margin) + (x._1-minX)*axisUnit, 
                                                   (size._2*(1.0-margin)) - (x._2-minY)*axisUnit,
                                                   5.0, // size
                                                   5.0 // size   
                                                 )
                                               ) 
                                   )
                                   
  // draw the regression line
  g.setColor(Color.BLUE)
  g.draw(new Line2D.Double(
                             margin*size._1,
                             size._2*(1.0-margin) - axisUnit*(model.getEquation.get(0) +
                                                              model.getEquation.get(1)*minX - 
                                                              minY),
                             (maxX-minX)*axisUnit + margin*size._1,
                             size._2*(1.0-margin) - axisUnit*(model.getEquation.get(0)+
                                                              model.getEquation.get(1)*maxX - 
                                                              minY)
                          )
  )
  
  // TODO: write the regression equation
  
  g.dispose()
  
  javax.imageio.ImageIO.write(canvas, 
                              "png", 
                              new java.io.File("drawing.png"))
  
}