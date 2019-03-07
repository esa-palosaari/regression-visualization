package regressionViz

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith


class UnitTests {
  
  // Tests for an empty dataset
  @Test def noneDataPoints ()  
  {
    val emptyData = new Data()
    assertTrue("An empty dataset's points should return None", emptyData.getPoints == None)
  }

  @Test def noneDataName () 
  {
    val emptyData = new Data()
    assertTrue("A dataset without a name should return None", emptyData.name == None)    
  }
    
  @Test def noneDataVarNames () 
  {
    val emptyData = new Data()
    assertTrue("A dataset without variable names should return None", 
        emptyData.getVarNames == None)    
  }
 
  // tests with something inside a dataset
  @Test def someDataName () 
  {
    val someData = new Data()
    someData.initializeDataset(newName= Some("Dataset's name"))
    assertTrue("A dataset with a name data points should return the same name in " +
        "a wrapper. Instead got: " + someData.name.toString(), 
        someData.name == Some("Dataset's name"))    
  }
  
  @Test def someDataPointsExists () 
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(1.23, 3.1343, 2), Array(0, 1, 2))))
    assertTrue("A dataset with data some points given should have them", 
        someData.getPoints.isDefined)    
  }
  
  @Test def someDataPoints () 
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(1.23, 3.1343, 2), Array(0, 1, 2))))
//    for{
//      something <- someData.points
//      array <- something
//      point <- array
//    } println(point)
    assertTrue("A dataset with some data points should return the points", (
        someData.getPoints.get(0)(0) == 1.23 &&
        someData.getPoints.get(0)(1) == 3.1343 &&
        someData.getPoints.get(0)(2) == 2 &&
        someData.getPoints.get(1)(0) == 0 &&
        someData.getPoints.get(1)(1) == 1 &&
        someData.getPoints.get(1)(2) == 2) )    
  }  
  
  @Test def someDataVarNamesExist ()
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(1.23, 3.1343, 2), Array(0, 1, 2))))
    assertTrue("A dataset with some data points should have variable names", 
        someData.getVarNames.isDefined)
  }
 
  // get error when initializing a model without data
  @Test(expected=classOf[ IllegalArgumentException])
  def withoutDataAnyModelReturnsError()
  {
   new OLSModel(new Data())
   }
  
  @Test def getNoneWhenModelNameNotDefined ()
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(1.23, 3.1343, 2), Array(0, 1, 2))))
    val olsModel = new OLSModel(someData)
    assertTrue("The model should have None as name when none is given. " +
               "Instead got: " + olsModel.name.toString(),
               olsModel.name == None)
  }
  
  @Test def getTheRightModelNameWhenItIsGiven ()
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(1.23, 3.1343, 2), Array(0, 1, 2))))
    val olsModel = new OLSModel(someData)
    olsModel.name = Some("malli")
    assertTrue("The model should have None as name when none is given. " +
               "Instead got: " + olsModel.name.toString(),
               olsModel.name == Some())    
  }
  
  /* create a dataset with unequal number of values, 
   * handle missing in the model preprocessing
   */
  // create data with ((1, 1), (1))
  
  
  // use MaxValue for missing data
    
}