package regressionViz

import scala.math._

class ArrayMatrix {
  
  def printMatrix(A: Array[Array[Double]]) = 
  {
    for
    {
      row <- 0 until A(0).length      
      column <- 0 until A.length
    }
    {     
      print(" " + (100*A(column)(row)).round / 100.toDouble + " ")
      if(column == A.length - 1) print("\n")      
    }
  }
  
  /*
   * Ax multiplication, returns a vector
   */
  
  def multiplyMatrixAndVector(A: Array[Array[Double]], x: Array[Double]):
    Array[Double] = 
  {
      require(A.length == x.length)
      
      val commonLength = A.length
      val rowsC = A(0).length
      var C: Array[Double] = Array.ofDim(rowsC)      
      for (rowIndex <- 0 until rowsC) 
      {
        var sumProduct: Double = 0.0
        for (commonIndex <- 0 until commonLength)
        {
          sumProduct += A(commonIndex)(rowIndex)*x(commonIndex)
        }
        C(rowIndex) = sumProduct
      }
      
      return C    
  }  

  /*
   * Multiplies two matrices, returns a matrix
   */
  def multiplyMatrices(A: Array[Array[Double]], B: Array[Array[Double]]): 
    Array[Array[Double]] =
    {
      require(A.length == B(0).length)
      
      val commonLength = A.length
      val rowsC = A(0).length
      val colsC = B.length
      var C: Array[Array[Double]] = Array.ofDim(colsC, rowsC)
      
      for 
      {
        rowIndex <- 0 until rowsC
        colIndex <- 0 until colsC
      } 
      {
        var sumProduct: Double = 0.0
        for (commonIndex <- 0 until commonLength)
        {
          sumProduct += A(commonIndex)(rowIndex)*B(colIndex)(commonIndex)
        }
        C(colIndex)(rowIndex) = sumProduct
      }
      
      return C
    }
  
  /*
   * Inverts matrix A, returns the inverted matrix
   */
  def invertMatrix(A: Array[Array[Double]]): Array[Array[Double]] =
  {
    require(A.length == A(0).length)
    val size = A.length
    // Use Gauss-Method to get the inverse
    // Here's Java code
    // http://cljavacode.blogspot.com/2017/06/inverse-matrix-by-gauss-jordan.html

    // Create an augmented matrix and add an identity matrix
    // at the right side of the original matrix
    var augA: Array[Array[Double]] = Array.ofDim(size*2, size)
    for 
    {
      rowIndex <- 0 until size
    }
    {
      augA(rowIndex+size)(rowIndex) = 1.0
    }
    
    for 
    {
      col <- 0 until size            
      row <- 0 until size
    }
    {
      augA(col)(row) = A(col)(row)
    } 
    
    // get the max diagonal
    for(row <- 0 until size)
    {
      var maks: Double = 0.0
      var maxRow: Int = row
      for(column <- row until size)
      {
        if (abs(augA(column)(row)) > maks)
        {
          maks = abs(augA(column)(row))
          maxRow = column   
        }
      }
      if(maxRow != row)
      {
        // swap rows maxRow and row
        for (kolumn <- 0 until size*2)
        {
          val temporary = augA(kolumn)(maxRow)
          augA(kolumn)(maxRow) = augA(kolumn)(row)
          augA(kolumn)(row) = temporary
        }
      }
    }
    
//    println("max diagonal: ")
//    printMatrix(augA)
    
    // diagonalize the matrix
    for 
    {
      rowIndex <- 0 until size
      rowIndex2 <- 0 until size
    }
    {
      if (rowIndex2 != rowIndex)
      {
        val temporary = augA(rowIndex)(rowIndex2) / augA(rowIndex)(rowIndex)
        for (k <- 0 until 2*size)
          augA(k)(rowIndex2) -= augA(k)(rowIndex)*temporary
      }
    }
    
//    println("Diagonalizd: ")
//    printMatrix(augA)
    
    // divide by diagonal
    for (rowIndex <- 0 until size)
    {
      val temporary = augA(rowIndex)(rowIndex)
      
      for (colIndex <- size until 2*size)
      {
        augA(colIndex)(rowIndex) = augA(colIndex)(rowIndex)/temporary        
      }
      augA(rowIndex)(rowIndex) = augA(rowIndex)(rowIndex)/temporary
    }
    
//    println("Divided: ")
//    printMatrix(augA)
    
    // take the inverse out
    var inverseA: Array[Array[Double]] = Array.ofDim(size, size) 
    for 
    {
      rowIndex <- 0 until size
      colIndex <- size until 2*size
    }
    {
      inverseA(colIndex-size)(rowIndex) = augA(colIndex)(rowIndex)      
    }
    return inverseA
  }    
  

}