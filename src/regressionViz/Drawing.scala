package regressionViz

// using this example for AWT: http://otfried.org/scala/drawing.html

import java.awt.image	.BufferedImage
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
  val margin = 0.15
  
  // plot size X and Y direction
  val plotSizeX = size._1.toDouble*(1.0-(margin*2.0))
  val plotSizeY = size._2.toDouble*(1.0-(2*margin))
  
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
  

  // get the order of the variable values
  val orderMaxX = abs(roundToMagnitude(maxX))
  val orderMinX = abs(roundToMagnitude(minX))
  val orderMaxY = abs(roundToMagnitude(maxY))
  val orderMinY = abs(roundToMagnitude(minY))
  val orderX = max(orderMaxX, orderMinX)
  val orderY = max(orderMaxY, orderMinY)
  
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
  
  // calculate the end points
  // dependent on whether x and y max and min given
  val smallestTickX = 
    {
      if(xmin.isEmpty) 
        (minX/orderX).toInt*orderX - orderX
      else
        minX
    }
  val largestTickX = 
    {
      if(xmax.isEmpty)
        (maxX/orderX).toInt*orderX + orderX
      else
        maxX
    }
  val smallestTickY = 
    {
      if(ymin.isEmpty)
        (minY/orderY).toInt*orderY - orderY
      else
        minY
    }
  val largestTickY = 
    {
      if(ymax.isEmpty)
        (maxY/orderY).toInt*orderY + orderY
      else
        maxY
    }
     
  
  val axisXUnit = (plotSizeX/(abs(largestTickX-smallestTickX)))
  val axisYUnit = (plotSizeY/(abs(largestTickY-smallestTickY)))
  val axisUnit = min(axisXUnit, axisYUnit)

  
  // how many ticks smaller is max point or curve than the image boundary? 
  // TODO: Fix extraTicks
  val extraTicksX = 
    {
      if(xmax.isDefined)
        0
      else
      (((plotSizeX - ((abs(maxX - minX)/orderX)*orderX *axisUnit)))/axisUnit).toInt
    }

  val extraTicksY =
    {
      if(ymax.isDefined)
        0
      else
        ((plotSizeY - (abs(maxY - minY)/orderY)*orderY*axisUnit)/axisUnit).toInt
    }
                 
  // number of axisUnits that "fit" on the coordinate axes
  val numberOfUnitsX = (plotSizeX/axisUnit).floor.toInt
  val numberOfUnitsY = (plotSizeY/axisUnit).floor.toInt
  

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
                            yWidth - axisUnit*smallestTickX,                        
                            yHiHeight,
                            yWidth - axisUnit*smallestTickX,                        
                            yLoHeight
                          )
  )
  // x-axis from origin
  g.draw(new Line2D.Double(
                             xLeftWidth,
                             xHeight + axisUnit*smallestTickY,
                             xRightWidth,
                             xHeight + axisUnit*smallestTickY
                           )
  )
  
  // write numbers on x- and y-axes
  g.setFont(new Font("Arial", Font.PLAIN, 12))        
  // write the tick numbers on the plot axes
  // numbers to x-axis
  var index = largestTickX + extraTicksX - 1
  while (index >= smallestTickX)
  {
    g.drawString(  
                  index.toInt.toString, 
                  (xLeftWidth  + ((index-smallestTickX)*axisUnit)).toInt, 
                  ((1-0.7*margin)*size._2).toInt
                )
    index -= orderX
  }
  // numbers to y-axis  
  index = largestTickY + extraTicksY   
  while (index >= smallestTickY)
  {
    g.drawString(  
                  index.toInt.toString, 
                  (0.4*yWidth).toInt, 
                  (yLoHeight - (index-smallestTickY)*axisUnit).toInt
                )
    index -= orderY
  }  
  

  // get the order of the variable values
  
  // write the regression equation
  g.setColor(new Color(0,0,0)) // set to black
  g.drawString(  model.equationToString, 
                 yWidth.toInt, 
                 ((1.0-0.1*margin)*size._2).toInt
              )
              
  // write the names of variables
  g.drawString(  model.getFittedData.get.getVarNames.get(0), 
                 size._1/2, 
                 ((1.0-0.4*margin)*size._2).toInt
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
                                                   xLeftWidth + (x._1-smallestTickX)*axisUnit, 
                                                   xHeight - (x._2-smallestTickY)*axisUnit,
                                                   5.0, // size
                                                   5.0 // size   
                                                 )
                                               ) 
                                   )
                                   
  // draw the regression curve
  g.setColor(new Color( curveColorR.getOrElse(0),
                        curveColorG.getOrElse(0),
                        curveColorB.getOrElse(255)
                      )
            )
       
  var xIndex = smallestTickX          
  while(xIndex <= (largestTickX + extraTicksX))
  {
    val point1Xcoord = yWidth + (xIndex - smallestTickX)*axisUnit
    val point1Ycoord = xHeight - axisUnit*(model.getY(xIndex) - smallestTickY)
    val point2Xcoord = (xIndex+(orderMinX/50.0)-smallestTickX)*axisUnit + yWidth
    val point2Ycoord = xHeight - axisUnit*(model.getY(xIndex+(orderMinX/50.0))- smallestTickY)
    if(point1Ycoord < yLoHeight && 
       point2Ycoord < yLoHeight && 
       point1Ycoord > yHiHeight &&
       point2Ycoord > yHiHeight &&
       point1Xcoord < xRightWidth)
    {
        g.draw(new Line2D.Double(
                                 point1Xcoord,
                                 point1Ycoord,
                                 point2Xcoord,
                                 point2Ycoord
                                )
        ) 
    }

    xIndex += (orderMinX/50.0)
  }
  
  
  

  
  g.dispose()
  
  
}