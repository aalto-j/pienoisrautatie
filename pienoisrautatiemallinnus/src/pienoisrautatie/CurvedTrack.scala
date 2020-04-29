package pienoisrautatie

import pienoisrautatie.constants._
import scala.math._
import java.awt.Graphics2D
import java.awt.geom._

class CurvedTrack(val r: Railway) extends Track(r) {

  var angle = 45
  var radius = 100
    
  this.joints = this.joints :+ new Joint(this, 1, None)
  this.joints = this.joints :+ new Joint(this, 2, None)
  
  //The facing direction of the joints

  def jointAngles = List[Int](this.rotation + 90, this.rotation + (270-this.angle))
  
  def setRadius(rot: Int) = this.rotation = rot
    
  def getArea = {
    val aT = new AffineTransform
    aT.rotate(toRadians(this.rotation), this.centerLocationX, this.centerLocationY)
    aT.translate((this.centerLocationX - this.radius - trackWidth/2 - cos(toRadians(this.angle/2))*this.radius), (this.centerLocationY - this.radius - trackWidth/2 + sin(toRadians(this.angle/2))*this.radius))    
    val a1 = new Area(new Arc2D.Double(0, 0, 2*this.radius + trackWidth, 2*this.radius + trackWidth, 0, this.angle, Arc2D.PIE))
    val a2 = new Area(new Arc2D.Double(24  , 24 , 2*this.radius - trackWidth, 2*this.radius - trackWidth, 0, this.angle, Arc2D.PIE))
    a1.subtract(a2)
    aT.createTransformedShape(a1)
  }
  
  override def changeAngle = {
    if (this.angle == 45) {this.angle = 30} else this.angle = 45
    this.checkStatus
  }
  
  override def changeLength(extend: Int) ={
    if ( (this.radius > 85 || extend == 1) && ( this.radius < 295 || extend == -1)) {this.radius += extend * 5}
    this.checkStatus
  }
  
  def trackToList = {
    if (this.angle == 30) {List[String]("3", this.centerLocationX.toString(), this.centerLocationY.toString(), this.rotation.toString(), this.radius.toString())
    } else {List[String]("4", this.centerLocationX.toString(), this.centerLocationY.toString(), this.rotation.toString(), this.radius.toString())}
  }
  
  //Position of joints
  
  def connectionPoints: List[(Int, Int)] = List[(Int, Int)]( ((this.centerLocationX + ((-cos(toRadians(-this.rotation + this.angle/2)) + cos(toRadians(-this.rotation)))*(this.radius))).toInt,              (this.centerLocationY +((sin(toRadians(-this.rotation + this.angle/2)) - sin(toRadians(-this.rotation)))*(this.radius))).toInt),
                                                             ((this.centerLocationX + ((-cos(toRadians(-this.rotation + this.angle/2)) + cos(toRadians(-this.rotation + this.angle)))*(this.radius))).toInt, (this.centerLocationY +((sin(toRadians(-this.rotation + this.angle/2)) - sin(toRadians(-this.rotation + this.angle)))*(this.radius))).toInt))
  
  def draw(g: Graphics2D) = {
    if (this.isSelected) {g.setColor(yellow)} else if (this.overlaps || !this.haveEnoughSpace) {g.setColor(red)} else {g.setColor(green)}
    val g2d: Graphics2D = g
    val aT = g2d.getTransform()
    g2d.fill(this.getArea)
    g2d.setTransform(aT)
  }
  
  override def toString() = this.railway.curvedDesc
}