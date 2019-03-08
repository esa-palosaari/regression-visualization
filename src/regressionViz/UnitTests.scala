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
  @Test def someDataNameShouldReturnTheSame () 
  {
    val someData = new Data()
    someData.initializeDataset(newName= Some("Dataset's name"))
    assertTrue("A dataset with a name data points should return the same name in " +
        "a wrapper. Instead got: " + someData.name.toString(), 
        someData.name == Some("Dataset's name"))    
  }
  
  @Test def someDataPointsShouldExist () 
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(1.23, 3.1343, 2), Array(0, 1, 2))))
    assertTrue("A dataset with data some points given should have them", 
        someData.getPoints.isDefined)    
  }
  
  @Test def someDataPointsShouldBeAsGiven () 
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(1.23, 3.1343, 2), Array(0, 1, 2))))
    assertTrue("A dataset with some data points should return the points", (
        someData.getPoints.get(0)(0) == 1.23 &&
        someData.getPoints.get(0)(1) == 3.1343 &&
        someData.getPoints.get(0)(2) == 2 &&
        someData.getPoints.get(1)(0) == 0 &&
        someData.getPoints.get(1)(1) == 1 &&
        someData.getPoints.get(1)(2) == 2) )    
  }  
  
  @Test def someDataVarNamesShouldExist ()
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
  
  @Test def shouldGetNoneWhenModelNameNotDefined ()
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(1.23, 3.1343, 2), Array(0, 1, 2))))
    val olsModel = new OLSModel(someData)
    assertTrue("The model should have None as name when none is given. " +
               "Instead got: " + olsModel.name.toString(),
               olsModel.name == None)
  }
  
  @Test def shouldGetTheRightModelNameWhenItIsGiven ()
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(1.23, 3.1343, 2), Array(0, 1, 2))))
    val olsModel = new OLSModel(someData)
    olsModel.name = Some("malli")
    assertTrue("The model should have None as name when none is given. " +
               "Instead got: " + olsModel.name.toString(),
               olsModel.name == Some("malli"))    
  }
  
  /* create a dataset with unequal number of values, 
   * handle missing in the model preprocessing
   */
 // get error when initializing a model without unequal number of data
  @Test(expected=classOf[ IllegalArgumentException])
  def unequal_Number_Of_Datapoints_Should_Produce_Error ()
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(1.23, 3.1343, 2), Array(0, 1))))
  }
  
  /*
   * Tests for the equation and residuals of the OLS model
   */
  
  @Test def should_Get_None_From_Equation_And_Residuals_When_Not_Fitted ()
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(1.23, 3.1343, 2), Array(0, 1, 2))))
    val olsModel = new OLSModel(someData)
    assertTrue(
        "The equation should be None when model is not fitted. " +
        "Instead, got: " + olsModel.getEquation.toString(),
        olsModel.getEquation == None)
    assertTrue(
        "The residuals should be None when model is not fitted. " +
        "Instead, got: " + olsModel.getResiduals.toString(),
        olsModel.getResiduals == None
        )        
  }
  
  @Test def equation_and_residuals_should_exist_when_an_OLS_Model_fitted ()
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(1.23, 3.1343, 2), Array(0, 1, 2))))
    val olsModel = new OLSModel(someData)
    olsModel.fitData
    assertTrue(
        "The regression equation should exist after OLS model is fitted. " +
        "Instead equation is: " + olsModel.getEquation.toString,
        olsModel.getEquation.isDefined
        )
    assertTrue(
        "The regression residuals should exist after OLS model is fitted.",
        olsModel.getResiduals.isDefined
        )    
        
  }
  
  // use MaxValue for missing data
  // create data with ((1, 1), (1, MaxValue))
  // OLSModel should take the row out when fitting a model
  @Test def OLSModel_should_have_listwise_deletion_and_require_enough_data ()
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(1.23, 3.1343), Array(0, Double.MaxValue))))
    val olsModel = new OLSModel(someData)
    olsModel.fitData
    assertTrue(
        "The OLS model should not be fitted when there are not enough rows. " +
        "Instead equation is: " + olsModel.getEquation.toString,
        !olsModel.getEquation.isDefined
        )
  }
  
  @Test def OLSModel_should_not_have_fittedData_before_fitting ()
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(1.23, 3.1343), Array(0, Double.MaxValue))))
    val olsModel = new OLSModel(someData)
    assertTrue(
        "There is fittedData before fitting: " + olsModel.getFittedData.toString,
        olsModel.getFittedData.isEmpty)
  }
  
  @Test def OLSModel_should_have_listwise_deletion ()
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(1.23, 3.1343), Array(0, Double.MaxValue))))
    val olsModel = new OLSModel(someData)
    olsModel.fitData
    assertTrue(
        "The OLS model should correctly delete rows with missing data. " +
        "Instead fittedData is: " + olsModel.getFittedData.toString,
        olsModel.getFittedData.get.getPoints == Some(Array(Array(1.23), Array(0)))
        )
  }
    
  
}