package utils
import numsca.Matrix

object Utils{
  def sigmoid(matrix: Matrix): Matrix = 
    val result = new Matrix(matrix.rows, matrix.cols)
    for i <- 0 until matrix.rows do 
      for j <- 0 until matrix.cols do 
        result(i,j) = 1 / (1 + math.exp(-matrix(i,j)))
    result
  
  def relu(matrix: Matrix): Matrix = 
    val result = new Matrix(matrix.rows, matrix.cols)
    for i <- 0 until matrix.rows do 
      for j <- 0 until matrix.cols do 
        if matrix(i,j) > 0 then
          result(i,j) = matrix(i,j)
        else 
          result(i,j) = 0
    result
  
  def sigmoid_backward(dA: Matrix, A: Matrix): Matrix = 
    val result = new Matrix(A.rows, A.cols)
      for i <- 0 until A.rows do 
        for j <- 0 until A.cols do   
          val a = A(i,j)
          val da = dA(i,j)
          result(i,j) = da * a * (1 - a) 
    result
  
  def relu_backward(dA: Matrix, A: Matrix): Matrix = 
    val result = new Matrix(A.rows, A.cols)
      for i <- 0 until A.rows do 
        for j <- 0 until A.cols do   
          val a = A(i,j)
          val da = dA(i,j)
          if a > 0 then 
            result(i,j) = da 
          else 
            result(i,j) = 0.0
    
    result

  
}

