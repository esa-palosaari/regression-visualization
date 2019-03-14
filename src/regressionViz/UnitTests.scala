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
  // create data with ((1, 1), (1, Double.NaN))
  // OLSModel should take the row out when fitting a model
  @Test(expected=classOf[ IllegalArgumentException])
  def OLSModel_should_require_enough_data ()
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(1.23, 3.1343), Array(0, Double.NaN))))
    val olsModel = new OLSModel(someData)
    olsModel.fitData
//    assertTrue(
//        "The OLS model should not be fitted when there are not enough rows. " +
//        "Instead equation is: " + olsModel.getEquation.get(0) +
//        "\nand there are " + olsModel.getFittedData.get.getPoints.get(0).length + 
//        " rows of data. Second column: \n" +
//        olsModel.getFittedData.get.getPoints.get(1).map(println(_)),
//        !olsModel.getEquation.isDefined
//        )
  }
  
  @Test def OLSModel_should_not_have_fittedData_before_fitting ()
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(1.23, 3.1343), Array(0, Double.NaN))))
    val olsModel = new OLSModel(someData)
    assertTrue(
        "There is fittedData before fitting: " + olsModel.getFittedData.toString,
        olsModel.getFittedData.isEmpty)
  }
  
  @Test(expected=classOf[ IllegalArgumentException])
  def OLSModel_should_have_listwise_deletion ()
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(1.23, 3.1343), Array(0.0, Double.NaN))))
    val olsModel = new OLSModel(someData)
    olsModel.fitData
//    assertTrue(
//        "The OLS model should correctly delete rows with missing data. " +
//        "Instead fittedData is: " + olsModel.getFittedData.get.getPoints.get(0)(0) +
//        " " + olsModel.getFittedData.get.getPoints.get(1)(0),
//        olsModel.getFittedData.get.getPoints.get(0)(0) == 1.23 && 
//        olsModel.getFittedData.get.getPoints.get(1)(0) == 0.0
//        )
  }
  
  @Test def OLSModel_should_give_the_correct_coefficients ()
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(0.0, 1.0), 
                                                      Array(2.0, 1.0),
                                                      Array(3.0, 4.0))))
    val olsModel = new OLSModel(someData)
    olsModel.fitData
    assertTrue(
      "The OLSModel should give the coefficients 0.571 and 0.857.\n" +
      "Instead got " + olsModel.getEquation.get(0) + " and " + 
      olsModel.getEquation.get(1),
      olsModel.getEquation.get(0) >= 0.571 &&
      olsModel.getEquation.get(0) < 0.572 &&
      olsModel.getEquation.get(1) >= 0.857 &&
      olsModel.getEquation.get(1) < 0.858     
    )
  }
  
  @Test def OLSModel_invertMatrix_should_give_correct_matrix ()
  {
    val someData = new Data()
    someData.initializeDataset(newPoints = Some(Array(Array(0.0, 1.0), 
                                                      Array(2.0, 1.0),
                                                      Array(3.0, 4.0))))
    val olsModel = new OLSModel(someData) 
    val A: Array[Array[Double]] = Array(Array(3, 3.2), Array(3.5, 3.6))
    val invertedMatrix: Array[Array[Double]] = olsModel.invertMatrix(A)
    assertTrue(
        "\nInstead of -9.0, A(0,0) is " + invertedMatrix(0)(0) +
        "\n instead of 8.0, A(0,1) is " + invertedMatrix(0)(1) +
        "\n instead of 8.75, A(0,1) is " + invertedMatrix(1)(0) +
        "\n instead of -7.5, A(0,1) is " + invertedMatrix(1)(1),
    invertedMatrix(0)(0) == -9.0 &&
    invertedMatrix(0)(1) == 8.0 &&
    invertedMatrix(1)(0) == 8.75 &&
    invertedMatrix(1)(1) == -7.5
    )
  }
    
  
}