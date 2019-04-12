package regressionViz

// trying things out for now
// using this example: http://otfried.org/scala/drawing.html as the basis

import java.awt.image.BufferedImage
import java.awt.{Graphics2D, Color, Font, BasicStroke}
import java.awt.geom._
import scala.math._

class Drawing (  val model: Model, 
                 val sizex: Option[Int], 
                 val sizey: Option[Int],
                 var xmax: Option[Int],
                 var xmin: Option[Int],
                 var ymax: Option[Int],
                 var ymin: Option[Int],
                 val pointColorR: Option[Int],
                 val pointColorB: Option[Int],
                 val pointColorG: Option[Int],
                 val curveColorR: Option[Int],
                 val curveColorB: Option[Int],
                 val curveColorG: Option[Int]
              )
{
  require(model.getFittedData.isDefined &&
          model.getEquation.isDefined)
  // image size
  val size = (sizex.getOrElse(500), sizey.getOrElse(500))
  
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
  val maxX: Int = xmax.getOrElse(x.max.toInt)
  val minX: Int = xmin.getOrElse(x.min.toInt)
  val maxY: Int = ymax.getOrElse(
                                  max(y.max.toInt, 
                                      max(model.getY(maxX),
                                          model.getY(minX)
                                         ).toInt
                                     )
                                 )
  val minY: Int = ymin.getOrElse(
                                    min(y.min.toInt, 
                                        min(model.getY(maxX),
                                            model.getY(minX)
                                           ).toInt
                                        )
                                 )
                                 
  val xHeight = size._2*(1.0- margin)
  val xLeftWidth = size._1*margin
  val xRightWidth = size._1*(1.0-margin)
  val yWidth = size._1*margin
  val yHiHeight = size._2*margin
  val yLoHeight = size._2*(1.0-margin)
  
  val axisXUnit: Int = ((size._1.toDouble*(1.0-(margin*2.0)))/
                         max(abs(maxX-minX),abs(minX-maxX))).floor.toInt
    
  val axisYUnit: Int = (size._2.toDouble*(1.0-(margin*2.0))/
                        max(abs(maxY-minY), abs(minY-maxY))).floor.toInt
                        
  val axisUnit: Int = min(axisXUnit, axisYUnit)


  // draw coordinate lines
  g.setStroke(new BasicStroke())
  g.setColor(new Color(0,0,0)) // set to black
  g.draw(new Line2D.Double(
                             xLeftWidth,
                             xHeight, 
                             xRightWidth,
                             xHeight
                          )
  )
  g.draw(new Line2D.Double(
                              yWidth, 
                              yHiHeight, 
                              yWidth, 
                              yLoHeight
                           )
  )
         
  // draw Cartesian x- and y-axes from origin, may not be visible
  g.setColor(Color.GRAY)
  // y-axis from origin
  g.draw(new Line2D.Double(
                            yWidth - axisUnit*minX,                        
                            yHiHeight,
                            yWidth - axisUnit*minX,                        
                            yLoHeight
                           )
  )
  // x-axis from origin
  g.draw(new Line2D.Double(
                             xLeftWidth,
                             xHeight + axisUnit*minY,
                             xRightWidth,
                             xHeight + axisUnit*minY
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
  var orderMaxX = abs(roundToMagnitude(maxX))
  var orderMinX = abs(roundToMagnitude(minX))
  var orderMaxY = abs(roundToMagnitude(maxY))
  var orderMinY = abs(roundToMagnitude(minY))
  var orderX = max(orderMaxX, orderMinX)
  var orderY = max(orderMaxY, orderMinY)
  
  // from https://stackoverflow.com/questions/7906996/algorithm-to-round-to-the-next-order-of-magnitude-in-r
  def roundToMagnitude(n: Double): Double =
  {
    if (n == 0) return 1
    val negative: Boolean = n < 0
    val logarithm = log10(abs(n))
    val decimalPlaces = logarithm.floor
    val rounded = pow(10, decimalPlaces)
    if (negative) return -rounded else return rounded
  }
  // get the rounded end points 
  val smallestTickX = (minX/orderX).toInt - orderX
  val largestTickX = (maxX/orderX).toInt + orderX
  val smallestTickY = (minY/orderY).toInt - orderY
  val largestTickY = (maxY/orderY).toInt + orderY
  
  // write the numbers from the smallest to the largest
  // how many ticks smaller is maX than the image boundary? 
  // TODO: check if extraTick works with negative maxX
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
                  (xLeftWidth  + (1+ index*pow(10,orderX))*axisUnit).toInt, // + abs(extraTicksX)*axisUnit
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
  g.setColor(new Color( pointColorR.getOrElse(255),
                        pointColorG.getOrElse(0),
                        pointColorB.getOrElse(0)
                      )
             )
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
  g.setColor(new Color( curveColorR.getOrElse(0),
                        curveColorG.getOrElse(0),
                        curveColorB.getOrElse(255)
                      )
            )
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
  
  
}