package pienoisrautatie

import pienoisrautatie.constants._
import java.awt.Graphics2D
import java.awt.Color
import java.awt.geom._
import scala.math._

class StraightTrack(val r:Railway) extends Track(r){
  
  var length  = 200
  
  this.joints = this.joints :+ new Joint(this, 1, None)
  this.joints = this.joints :+ new Joint(this, 2, None)
  
  //The facing direction of the joints
  
  def jointAngles = List[Int](this.rotation, this.rotation + 180)
  
  def extend = {
    if (this.length < straightMaxLength) {this.length += 5}
    this.checkStatus
  }
  
  def shrink = {
    if (this.length > straightMinLength) {this.length -= 5}
    this.checkStatus
  }
  
  //Position of joints
  
  def connectionPoints: List[(Int, Int)] = List[(Int, Int)](((this.centerLocationX + (cos(toRadians(this.rotation))*length/2)).toInt, (this.centerLocationY + (sin(toRadians(this.rotation))*length/2)).toInt),
                                                            ((this.centerLocationX - cos(toRadians(this.rotation))*length/2).toInt, (this.centerLocationY - (sin(toRadians(this.rotation))*length/2).toInt)))
  def setLength(len: Int) = this.length = len
  
  override def changeLength(extend: Int) = if ((this.length > 15 || extend == 1) && ( this.length < 295 || extend == -1)) {this.length += extend * 5}
  
  def trackToList = List[String]("S", this.centerLocationX.toString(), this.centerLocationY.toString(), this.rotation.toString(), this.length.toString())
  
  def getArea() = {
   val rect = new Rectangle2D.Double(this.centerLocationX - this.length / 2, this.centerLocationY - trackWidth / 2, this.length, trackWidth)
   val aT = new AffineTransform
   aT.rotate(toRadians(this.rotation), this.centerLocationX, this.centerLocationY)
   aT.createTransformedShape(rect)
  }
    
  def draw(g: Graphics2D) = {    
    if (this.isSelected) {g.setColor(yellow)} else if (this.overlaps || !this.haveEnoughSpace) {g.setColor(red)} else {g.setColor(green)}
    val g2d: Graphics2D = g
    val aT = g2d.getTransform()
    g2d.fill(this.getArea)
    g2d.setTransform(aT)
  }
 
  override def toString() = this.railway.straightDesc
}