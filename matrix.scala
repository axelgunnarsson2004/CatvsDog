package numsca 
import scala.util.Random
import java.awt.{Color, Graphics2D}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File

class Matrix(val rows: Int, val cols: Int, val elements: Array[Array[Double]]){
  val shape = (rows,cols)
  
  def this(rows: Int, cols: Int) = this(rows,cols, Array.ofDim[Double](rows,cols))
  
  def apply(i: Int, j: Int): Double = elements(i)(j)

  def update(i: Int, j: Int, value: Double): Unit = 
    elements(i)(j) = value
  
  //Matrix addition
  def +(that: Matrix): Matrix = {
    // Case when matrix dimensions match exactly
    if (this.rows == that.rows && this.cols == that.cols) {
      val result = new Matrix(this.rows, this.cols, Array.fill(this.rows, this.cols)(0.0))
      for (i <- 0 until this.rows) {
        for (j <- 0 until this.cols) {
          result(i, j) = this(i, j) + that(i, j)
        }
      }
      return result
    }
    
    // Case when that matrix has a single column (rows, 1), we broadcast across each column of the original matrix
    if (this.rows == that.rows && that.cols == 1) {
      val result = new Matrix(this.rows, this.cols, Array.fill(this.rows, this.cols)(0.0))
      for (i <- 0 until this.rows) {
        for (j <- 0 until this.cols) {
          result(i, j) = this(i, j) + that(i, 0) // Broadcast column-wise
        }
      }
      return result
    }

    // If dimensions don't match and are not broadcastable, throw an error
    throw new IllegalArgumentException(f"Matrix dimensions (${this.rows}, ${this.cols}) and (${that.rows}, ${that.cols}) are not compatible for addition")
  } 

  // def +(that: Matrix): Matrix = 
  //   if (this.cols != that.cols || this.rows != that.rows) then
  //     throw new IllegalArgumentException(f"Matrix dimensions (${this.rows},${this.cols}), (${that.rows},${that.cols}) must match for addition")
  //   val result = new Matrix(rows, cols)
  //   for i <- 0 until rows do 
  //     for j <- 0 until cols do 
  //       result(i,j) = this(i,j) + that(i,j)
  //   result

  def +(scalar: Double): Matrix = 
    val result = new Matrix(rows, cols)
    for i <- 0 until rows do 
      for j <- 0 until cols do 
        result(i,j) = this(i,j) + scalar
    result
  //Matrix subtraction 
  def -(that: Matrix): Matrix = 
    if (this.cols != that.cols || this.rows != that.rows) then
      throw new IllegalArgumentException("Matrix dimensions must match for subtraction")
    val result = new Matrix(rows, cols)
    for i <- 0 until rows do 
      for j <- 0 until cols do 
        result(i,j) = this(i,j) - that(i,j)
    result
  
  def subtractTo(scalar: Double): Matrix = 
    val result = new Matrix(rows, cols)
    for i <- 0 until rows do 
      for j <- 0 until cols do 
        result(i,j) = 1 - this(i,j)
    result 
  
  //Elementwise multiplication
  def *(that: Matrix): Matrix = 
    if (this.cols != that.cols || this.rows != that.rows) then
      throw new IllegalArgumentException("Matrix dimensions must match for element-wise multiplication")
    val result = new Matrix(rows, cols)
    for i <- 0 until rows do 
      for j <- 0 until cols do 
        result(i,j) = this(i,j) * that(i,j)
    result
 
  def /(that: Matrix): Matrix = 
    if (this.cols != that.cols || this.rows != that.rows) then
      throw new IllegalArgumentException("Matrix dimensions must match for element-wise multiplication")
    val result = new Matrix(rows, cols)
    for i <- 0 until rows do 
      for j <- 0 until cols do 
        result(i,j) = this(i,j) / that(i,j)
    result
 
  //Elementwise multiplication with scalar
  def *(scalar: Double): Matrix = 
    val result = new Matrix(rows, cols)
    for i <- 0 until rows do 
      for j <- 0 until cols do 
        result(i,j) = this(i,j) * scalar
    result
  
  
  def log: Matrix = 
    val result = new Matrix(rows, cols)
    for i <- 0 until rows do 
      for j <- 0 until cols do 
        result(i,j) = math.log(this(i,j))    
    result


  //Matrix multiplication
  infix def dot(that: Matrix): Matrix = 
    if (this.rows != that.cols) then
      throw new IllegalArgumentException(f"Matrix dimenension (${this.rows},${this.cols}) and (${that.rows},${that.cols}) doesnt match for dot multiplication")
    val result = new Matrix(this.rows,that.cols)
    for i <- 0 until this.rows do 
      for j <- 0 until that.cols do 
        for k <- 0 until this.cols do 
          result(i,j) = this(i,k) * that(k,j)
    result

  infix def matmul(that: Matrix) = 
    if (this.cols != that.rows) then
      throw new IllegalArgumentException(f"Matrix dimenension (${this.rows},${this.cols}) and (${that.rows},${that.cols}) doesnt match for matrix multiplication")
    val result = new Matrix(this.rows,that.cols)
    for i <- 0 until this.rows do 
      for j <- 0 until that.cols do 
        var sum = 0.0
        for k <- 0 until this.cols do 
          sum += this(i,k) * that(k,j)
        result(i,j) = sum
    result
  
  def sum(axis: Int): Matrix = 
    axis match 
      case 0 =>
        val result = Matrix(1,this.shape(1))
        for j <- 0 until this.cols do 
          var sum =0.0
          for i <- 0 until this.rows do 
            sum += this(i,j) 
          result(0,j) = sum
        result

      case 1 =>
        val result = Matrix(this.shape(0),1) 
        for i <- 0 until this.rows do 
          var sum = 0.0
            for j <- 0 until this.cols do 
              sum += this(i,j)
            result(i,0) = sum
        result

  def T: Matrix = 
    val result = new Matrix(cols,rows)
    for i <- 0 until rows do 
      for j <- 0 until cols do
        result(j,i) = this(i,j)
    result
  
  def plot(output: String): Unit = {
    // Check if the matrix has 12288 elements (64x64 image with 3 channels)
    if (rows != 12288 || cols != 1) {
      throw new IllegalArgumentException("Matrix must have shape (12288, 1) to be plotted as an RGB image.")
    }

    // Define the image size (64x64) and create a BufferedImage
    val imgWidth = 64
    val imgHeight = 64
    val img = new BufferedImage(imgWidth, imgHeight, BufferedImage.TYPE_INT_RGB)

    // Unnormalize the matrix elements and map them back to RGB values
    var idx = 0
    for (y <- 0 until imgHeight) {
      for (x <- 0 until imgWidth) {
        // Extract RGB values from the matrix (normalized between 0 and 1, so scale them to [0, 255])
        val red = (elements(idx)(0) * 255).toInt
        val green = (elements(idx + 1)(0) * 255).toInt
        val blue = (elements(idx + 2)(0) * 255).toInt
        idx += 3

        // Create a new Color from RGB values
        val color = new Color(red, green, blue)

        // Set the color for the pixel in the BufferedImage
        img.setRGB(x, y, color.getRGB)
      }
    }
    
    // Save the image as a .jpg file
    val file = new File(output)
    ImageIO.write(img, "jpg", file)

    println(s"Image saved as $output")
  }

   def stringMatrix:String  = 
    var s = "\n"
    for i <- 0 until rows do 
      for j <- 0 until cols do
        s = s + f"${elements(i)(j)}%1.2f" + " "
        
      s = s + "\n"  
    s
  override def toString = stringMatrix//"(" + rows.toString + "," + cols.toString + ")"
}

object Matrix{
  
  def randomMatrix(rows: Int, cols: Int): Matrix = 
    val random = new Random
    val elements = Array.ofDim[Double](rows,cols) 
    for i <- 0 until rows do 
      for j <- 0 until cols do 
        elements(i)(j) = standardNormal() //random.nextDouble()
    Matrix(rows,cols,elements)


  def fromArray(arr: Array[Array[Double]]): Matrix = 
    var rows = arr.length
    var cols = arr(0).length
    new Matrix(rows,cols,arr)

    

}



def standardNormal(): Double = 
  val random = Random
  val u1 = random.nextDouble()
  val u2 = random.nextDouble()

  val z0 = Math.sqrt(-2.0 * Math.log(u1)) * Math.cos(2.0 * Math.PI * u2)
  z0




