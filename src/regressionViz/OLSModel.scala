package regressionViz

class OLSModel (data: Data) extends Model (data)
{


  def fitData: Unit = 
  {
    // listwise deletion for missing values
    if (fittedData.isEmpty) checkAndDeleteMissingRows
    // check that there are enough rows to fit
    
    // calculate the normal equation
    
    // calculate the residuals
  }
  

  
}