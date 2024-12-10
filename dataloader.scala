import numsca.Matrix
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.collection.mutable.ArrayBuffer



class DataLoader(paths: String*) {

  // Method to load all images from the given directories
  def loadImages(): Array[BufferedImage] = {
    val images = ArrayBuffer[BufferedImage]()
    
    for (path <- paths) {
      val directory = new File(path)
      if (directory.exists && directory.isDirectory) {
        val imageFiles = directory.listFiles().filter(_.getName.toLowerCase.endsWith(".jpg"))
        for (file <- imageFiles) {
          val img = ImageIO.read(file)
          if (img != null) images += img
        }
      }
    }
    
    images.toArray
  }

  // Method to resize images to 64x64x3
  def resizeImage(image: BufferedImage, width: Int = 64, height: Int = 64): BufferedImage = {
    val resized = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val graphics = resized.createGraphics()
    graphics.drawImage(image, 0, 0, width, height, null)
    graphics.dispose()
    resized
  }

  // Method to extract pixel values as (red, green, blue) from a BufferedImage and normalize them
  def imageToNormalizedArray(image: BufferedImage): Array[Double] = {
    val width = image.getWidth
    val height = image.getHeight
    val pixels = new Array[Double](width * height * 3)
    
    var index = 0
    for (y <- 0 until height; x <- 0 until width) {
      val rgb = image.getRGB(x, y)
      val red = (rgb >> 16) & 0xFF
      val green = (rgb >> 8) & 0xFF
      val blue = rgb & 0xFF
      
      // Normalize to [0, 1]
      pixels(index) = red / 255.0
      pixels(index + 1) = green / 255.0
      pixels(index + 2) = blue / 255.0
      index += 3
    }
    
    pixels
  }

  // Main method to return a matrix of all processed images
  def processImages(): Matrix = {
    // Load all images
    val images = loadImages()
    
    // Process and normalize each image
    val processedImages = images.map { img =>
      val resized = resizeImage(img)
      imageToNormalizedArray(resized)
    }

    // Convert each processed image into a matrix row (each image is a row)
    
    // Create a matrix where each row represents one image's pixel data
    val imageData = processedImages.toArray // Array[Array[Double]]

    // Convert this into a Matrix where each row is an image and columns are pixel values
    Matrix.fromArray(imageData).T
  }
}

val cat_path ="archive/training_set/training_set/cats" 

def main = 
  var c = DataLoader(cat_path)  
  var im = c.processImages()

  Matrix.fromArray(Array(im.T.elements(1))).T.plot("test.jpg")
