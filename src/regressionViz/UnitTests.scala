package regressionViz

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith


class UnitTests {
  
  // Tests for an empty dataset
  @Test def noneDataPoints () {
    val emptyData = new Data()
    assertTrue("An empty dataset's points should return None", emptyData.points == None)
 
  }

  @Test def noneDataName () {
    val emptyData = new Data()
    assertTrue("A dataset without a name should return None", emptyData.name == None)    
  }
    
  @Test def noneDataVarNames () {
    val emptyData = new Data()
    assertTrue("A dataset without variable names should return None", emptyData.varNames == None)    
  }
 
  // tests with something inside a dataset
  @Test def someDataName () {
    val someData = new Data(name = Some("dataset's name"))
    assertTrue("A dataset with a name data points should return Some(name)", someData.name == Some("dataset's name"))    
  }
  
  @Test def someDataPointsExists () {
    val someData = new Data(points = Some(Array(Array(1.23, 3.1343, 2), Array(0, 1, 2))))
    assertTrue("A dataset with data some points should return points.isDefined == true", someData.points.isDefined == true)    
  }
  
  @Test def someDataPoints () {
    val someData = new Data(points = Some(Array(Array(1.23, 3.1343, 2), Array(0, 1, 2))))
//    for{
//      something <- someData.points
//      array <- something
//      point <- array
//    } println(point)
    assertTrue("A dataset with data some points should return the points", (
        someData.points.get(0)(0) == 1.23 &&
        someData.points.get(0)(1) == 3.1343 &&
        someData.points.get(0)(2) == 2 &&
        someData.points.get(1)(0) == 0 &&
        someData.points.get(1)(1) == 1 &&
        someData.points.get(1)(2) == 2) )    
  }  
 
  
  // create a dataset with unequal number of values  
  
  // create another dataset with unequal number of values
  
  // create 
}