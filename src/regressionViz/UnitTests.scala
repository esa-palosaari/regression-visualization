package regressionViz

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith


class UnitTests {
  
  // Tests for an empty dataset
  @Test def nonePoints () {
    val emptyData = new Data()
    assertTrue("An empty dataset's points should return None", emptyData.points == None)
 
  }

  @Test def noneName () {
    val emptyData = new Data()
    assertTrue("A dataset without a name should return None", emptyData.name == None)    
  }
    
  @Test def noneVarNames () {
    val emptyData = new Data()
    assertTrue("A dataset without data points should return None", emptyData.varNames == None)    
  }
      
  
  // create a dataset with unequal number of values
  
  // create another dataset with unequal number of values
  
  // create 
}