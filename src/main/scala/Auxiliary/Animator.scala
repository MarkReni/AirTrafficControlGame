package Auxiliary
import java.awt.geom.AffineTransform
import java.awt.image.{AffineTransformOp, BufferedImage}
import java.io.File
import javax.imageio.ImageIO
import scala.swing._


object Animator {
  // scales the picture given as a parameter
  def scalePic(picPath: String, width: Int, height: Int): Image = {
    val pic: BufferedImage = ImageIO.read(new File(picPath))
    val picScaled: Image = pic.getScaledInstance(width, height, java.awt.Image.SCALE_DEFAULT)
    picScaled
  }

  // rotates the picture given as a parameter
  def rotate(degrees: Int, file: String): BufferedImage = {
    // PICTURE ROTATION using Affine Transform
    val pic: BufferedImage = ImageIO.read(new File(file))
    val rads: Double = Math.toRadians(degrees)
    val sin: Double = Math.abs(Math.sin(rads))
    val cos: Double = Math.abs(Math.cos(rads))
    val width: Int = Math.floor(pic.getWidth * cos + pic.getHeight * sin).toInt
    val height: Int = Math.floor(pic.getHeight * cos + pic.getWidth * sin).toInt
    val typeImage = pic.getType
    val rotatedPic: BufferedImage = new BufferedImage(width, height, typeImage)
    val af = new AffineTransform()
    af.scale(0.4, 0.4)
    af.rotate(rads, width/2, height/2)
    val offset: Double = (width - height) / 2
    af.translate(-offset, -offset)
    af.translate(0, 0)
    val afo = new AffineTransformOp(af, AffineTransformOp.TYPE_BILINEAR)
    afo.filter(pic, rotatedPic)
  }


}
