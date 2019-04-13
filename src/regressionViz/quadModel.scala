package regressionViz

class quadModel (data: Data) extends Model(data) {
  def getY(x: Double): Double
  // TODO: a maximum and minimum Y for non-linear models?
  def fitData: Unit
}