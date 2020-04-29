package pienoisrautatie

import java.awt.Graphics2D
import java.awt.geom._
import pienoisrautatie.constants._
import scala.math._
import javax.imageio.ImageIO
import java.io.File

class EndTrack(val r: Railway) extends Track(r){
  
   this.joints = this.joints :+ new Joint(this, 1)

   //The facing direction of the joints
   
   def jointAngles = List[Int](this.rotation)
   
   def getArea = {
     val r1 = new Rectangle2D.Double(this.centerLocationX - trackWidth / 2, this.centerLocationY - trackWidth / 2, trackWidth, trackWidth)
     
     val aT = new AffineTransform
     aT.rotate(toRadians(this.rotation), this.centerLocationX, this.centerLocationY)
     aT.createTransformedShape(r1)
  }
    
   def getBlackRectangle = {
     val r2 = new Rectangle2D.Double(this.centerLocationX + 2, this.centerLocationY - 10, 4, 20)
     val aT = new AffineTransform
     aT.rotate(toRadians(this.rotation), this.centerLocationX, this.centerLocationY)
     aT.createTransformedShape(r2)
   }
   
   def getRedRectangle(adj: Int) = {
     val r3 = new Rectangle2D.Double(this.centerLocationX + 6, this.centerLocationY + adj, 3, 4)
     val aT = new AffineTransform
     aT.rotate(toRadians(this.rotation), this.centerLocationX, this.centerLocationY)
     aT.createTransformedShape(r3)
   }
   
    def draw(g: Graphics2D) = {
      if (this.isSelected) {g.setColor(yellow)} else if (this.overlaps || !this.haveEnoughSpace) {g.setColor(red)} else {g.setColor(green)}
      val g2d: Graphics2D = g
      val aT = g2d.getTransform()
      val at2 = new AffineTransform
      g2d.fill(this.getArea)
      g2d.setColor(black)
      g2d.fill(this.getBlackRectangle)
      g2d.setColor(red)
      g2d.fill(this.getRedRectangle(-8))
      g2d.fill(this.getRedRectangle(4))
      g2d.setTransform(aT)
    }
     
    //Position of joint
    
    def connectionPoints: List[(Int, Int)] = List[(Int, Int)](((this.centerLocationX + (cos(toRadians(this.rotation))*trackWidth/2)).toInt, (this.centerLocationY + (sin(toRadians(this.rotation))*trackWidth/2)).toInt))
    
    def trackToList = List[String]("E", this.centerLocationX.toString(), this.centerLocationY.toString(), this.rotation.toString())
    
    override def toString() = this.railway.endDesc
}
