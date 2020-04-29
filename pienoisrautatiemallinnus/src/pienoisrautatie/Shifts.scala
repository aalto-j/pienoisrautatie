package pienoisrautatie

import pienoisrautatie.constants._
import java.awt.Graphics2D
import java.awt.geom._
import scala.math._

abstract class ThreeEndShift(r: Railway) extends Track(r) {
  
  val radius = 285
  val angle  = 30
  val length = 150
  
  this.joints = this.joints :+ new Joint(this, 1)
  this.joints = this.joints :+ new Joint(this, 2)
  this.joints = this.joints :+ new Joint(this, 3)

  //The facing direction of the joints
  
  def jointAngles = List[Int](this.rotation, this.rotation + 180, this.rotation + 330)
    
  var goingStraight: Boolean = true
  
  override def makeShift = {
    this.goingStraight = !this.goingStraight
    this.railway.setCommentary("Shift made")
    this.checkStatus
  }
  
  override def neighbours = {
    if (this.goingStraight) {
      this.joints.filterNot(_.no == 3).map(_.pairedTo).filter(_ != None).map(_.get.owner)
    } else {this.joints.filterNot(_.no == 2).map(_.pairedTo).filter(_ != None).map(_.get.owner)}
  }
  
  def getRectangleArea = {
    val rect = new Rectangle2D.Double(this.centerLocationX - this.length / 2, this.centerLocationY - trackWidth / 2, this.length, trackWidth)
    val aT = new AffineTransform
    aT.rotate(toRadians(this.rotation), this.centerLocationX, this.centerLocationY)
    aT.createTransformedShape(rect)
  }
  
  def getArcArea = {
    val aT = new AffineTransform
    aT.rotate(toRadians(this.rotation + 90), this.centerLocationX, this.centerLocationY)    
    aT.translate((this.centerLocationX - 10 - this.radius - trackWidth/2 - cos(toRadians(this.angle/2))*this.radius), (this.centerLocationY +1 - this.radius - trackWidth/2 + sin(toRadians(this.angle/2))*this.radius))
    val a1 = new Area(new Arc2D.Double(0, 0, 2*this.radius + trackWidth, 2*this.radius + trackWidth, 0, this.angle, Arc2D.PIE))
    val a2 = new Area(new Arc2D.Double(24  , 24 , 2*this.radius - trackWidth, 2*this.radius - trackWidth, 0, this.angle, Arc2D.PIE))   
    a1.subtract(a2)
    aT.createTransformedShape(a1)
  }
  
  def getArea = {
    val aT = new AffineTransform
    val a1 = new Area(this.getRectangleArea)
    val a2 = new Area(this.getArcArea)
    a1.add(a2)
    aT.createTransformedShape(a1)
  }
  
  //Position of joints
  
  def connectionPoints: List[(Int, Int)] = List[(Int, Int)](((this.centerLocationX + cos(toRadians(this.rotation))*length/2).toInt, (this.centerLocationY + sin(toRadians(this.rotation))*length/2).toInt),
                                                            ((this.centerLocationX - cos(toRadians(this.rotation))*length/2).toInt, (this.centerLocationY - sin(toRadians(this.rotation))*length/2).toInt),
                                                            ((this.centerLocationX + cos(toRadians(this.rotation + 330))*length/2).toInt, (this.centerLocationY + sin(toRadians(this.rotation + 330))*length/2).toInt))
  
  
  def draw(g: Graphics2D) = {
    val g2d: Graphics2D = g
    val aT = g2d.getTransform()
    if (this.goingStraight) {
      if (this.isSelected) {g2d.setColor(lightYellow)} else if (this.overlaps || !this.haveEnoughSpace) {g2d.setColor(lightRed)} else {g2d.setColor(lightGreen)}
      g2d.fill(this.getArcArea)
      if (this.isSelected) {g2d.setColor(yellow)} else if (this.overlaps || !this.haveEnoughSpace) {g2d.setColor(red)} else {g2d.setColor(green)}
      g2d.fill(this.getRectangleArea)
    } else {
      if (this.isSelected) {g2d.setColor(lightYellow)} else if (this.overlaps || !this.haveEnoughSpace) {g2d.setColor(lightRed)} else {g2d.setColor(lightGreen)}
      g2d.fill(this.getRectangleArea)
      if (this.isSelected) {g2d.setColor(yellow)} else if (this.overlaps || !this.haveEnoughSpace) {g2d.setColor(red)} else {g2d.setColor(green)}
      g2d.fill(this.getArcArea)
    }
    g2d.setTransform(aT)
  }
  
}

class ThreeEndShiftL(val r: Railway) extends ThreeEndShift(r) {
  
  def trackToList = List[String]("TL", this.centerLocationX.toString(), this.centerLocationY.toString(), this.rotation.toString())
  
  override def toString() = this.railway.teslDesc + " Shift is leading straight: " + this.goingStraight
}

class ThreeEndShiftR(val r: Railway) extends ThreeEndShift(r) {
  
  def trackToList = List[String]("TR", this.centerLocationX.toString(), this.centerLocationY.toString(), this.rotation.toString())
  
  //The facing direction of the joints
  
  override def jointAngles = List[Int](this.rotation, this.rotation + 180, this.rotation + 210)
  
  override def getArcArea = {
    val aT = new AffineTransform
    aT.rotate(toRadians(this.rotation + 120), this.centerLocationX, this.centerLocationY)
    aT.translate((this.centerLocationX - 9 - this.radius - trackWidth/2 - cos(toRadians(this.angle/2))*this.radius), (this.centerLocationY + 4 - this.radius - trackWidth/2 + sin(toRadians(this.angle/2))*this.radius))
    val a1 = new Area(new Arc2D.Double(0, 0, 2*this.radius + trackWidth, 2*this.radius + trackWidth, 0, this.angle, Arc2D.PIE))
    val a2 = new Area(new Arc2D.Double(24  , 24 , 2*this.radius - trackWidth, 2*this.radius - trackWidth, 0, this.angle, Arc2D.PIE))   
    a1.subtract(a2)
    aT.createTransformedShape(a1)}
  
  //Position of joints
  
  override def connectionPoints: List[(Int, Int)] = List[(Int, Int)](((this.centerLocationX + cos(toRadians(this.rotation))*length/2).toInt, (this.centerLocationY + sin(toRadians(this.rotation))*length/2).toInt),
                                                            ((this.centerLocationX - cos(toRadians(this.rotation))*length/2).toInt, (this.centerLocationY - sin(toRadians(this.rotation))*length/2).toInt),
                                                            ((this.centerLocationX + cos(toRadians(this.rotation + 210))*length/2).toInt, (this.centerLocationY + sin(toRadians(this.rotation + 210))*length/2).toInt))
  
  
  override def toString() = this.railway.tesrDesc + " Shift is leading straight: " + this.goingStraight
}

class FourEndShift(val r: Railway) extends Track(r) {
  
  val length = 150
  val angle  = 60
  val radius = (this.length/2*cos(toRadians(this.angle/2)))/(sin(toRadians(this.angle/2)))
  
  this.joints = this.joints :+ new Joint(this, 1)
  this.joints = this.joints :+ new Joint(this, 2)
  this.joints = this.joints :+ new Joint(this, 3)
  this.joints = this.joints :+ new Joint(this, 4)
  
  var goingStraight: Boolean = true
  
  override def makeShift = this.goingStraight = !this.goingStraight
  
  def jointAngles = List[Int](this.rotation + 330, this.rotation + 30, this.rotation + 150, this.rotation + 210 )
  
  def trackToList = List[String]("F", this.centerLocationX.toString(), this.centerLocationY.toString(), this.rotation.toString())
  
  def getRectangleArea(dAng: Int) = {
    
    val rect = new Rectangle2D.Double(this.centerLocationX - this.length / 2, this.centerLocationY - trackWidth / 2, this.length, trackWidth)
    val aT = new AffineTransform
    aT.rotate(toRadians(this.rotation + dAng), this.centerLocationX, this.centerLocationY)
    aT.createTransformedShape(rect)
  }
  
  def getArcArea(dAng: Int, adjustX: Int, adjustY: Int) = {
    val aT = new AffineTransform
    aT.rotate(toRadians(this.rotation + dAng), this.centerLocationX, this.centerLocationY)   
    aT.translate((this.centerLocationX + adjustX - this.radius - trackWidth/2 - cos(toRadians(this.angle/2))*this.radius), (this.centerLocationY + adjustY - this.radius - trackWidth/2 + sin(toRadians(this.angle/2))*this.radius))
    val a1 = new Area(new Arc2D.Double(0, 0, 2*this.radius + trackWidth, 2*this.radius + trackWidth, 0, this.angle, Arc2D.PIE))
    val a2 = new Area(new Arc2D.Double(24  , 24 , 2*this.radius - trackWidth, 2*this.radius - trackWidth, 0, this.angle, Arc2D.PIE))
    a1.subtract(a2)
    aT.createTransformedShape(a1)
  }
  
  def getArea  ={
    val aT = new AffineTransform
    val r1 = new Area(this.getRectangleArea(30))
    val r2 = new Area(this.getRectangleArea(-30))
    val a1 = new Area(this.getArcArea(120, -17, 10))
    val a2 = new Area(this.getArcArea(-60, -19, 10))
    r1.add(r2)
    r1.add(a1)
    r1.add(a2)
    aT.createTransformedShape(r1)
  }
 
  //Position of joints
  
  def connectionPoints: List[(Int, Int)] = List[(Int, Int)](
                                                            ((this.centerLocationX + cos(toRadians(this.rotation - this.angle/2))*length/2).toInt, (this.centerLocationY + sin(toRadians(this.rotation - this.angle/2))*length/2).toInt),                                                      
                                                            ((this.centerLocationX + cos(toRadians(this.rotation + this.angle/2))*length/2).toInt, (this.centerLocationY + sin(toRadians(this.rotation + this.angle/2))*length/2).toInt),
                                                            ((this.centerLocationX - cos(toRadians(this.rotation - this.angle/2))*length/2).toInt, (this.centerLocationY - sin(toRadians(this.rotation - this.angle/2))*length/2).toInt),                                                      
                                                            ((this.centerLocationX - cos(toRadians(this.rotation + this.angle/2))*length/2).toInt, (this.centerLocationY - sin(toRadians(this.rotation + this.angle/2))*length/2).toInt))
  
  def draw(g: Graphics2D) = {
    if (this.isSelected) {g.setColor(yellow)} else {g.setColor(green)}
    val g2d = g
    val aT = g2d.getTransform()
    
    if (this.goingStraight) {
      if (this.isSelected) {g2d.setColor(lightYellow)} else if (this.overlaps || !this.haveEnoughSpace) {g2d.setColor(lightRed)} else {g2d.setColor(lightGreen)}
      g2d.fill(new Area(this.getArcArea(120, -17, 10)))
      g2d.fill(new Area(this.getArcArea(-60, -19, 10)))
      if (this.isSelected) {g2d.setColor(yellow)} else if (this.overlaps || !this.haveEnoughSpace) {g2d.setColor(red)} else {g2d.setColor(green)}
      g2d.fill(new Area(this.getRectangleArea(30)))
      g2d.fill(new Area(this.getRectangleArea(-30)))
    } else {
      if (this.isSelected) {g2d.setColor(lightYellow)} else if (this.overlaps || !this.haveEnoughSpace) {g2d.setColor(lightRed)} else {g2d.setColor(lightGreen)}
      g2d.fill(new Area(this.getRectangleArea(30)))
      g2d.fill(new Area(this.getRectangleArea(-30)))
      if (this.isSelected) {g2d.setColor(yellow)} else if (this.overlaps || !this.haveEnoughSpace) {g2d.setColor(red)} else {g2d.setColor(green)}
      g2d.fill(new Area(this.getArcArea(120, -17, 10)))
      g2d.fill(new Area(this.getArcArea(-60, -19, 10)))
    }
    
    g2d.setTransform(aT)
    
    
  }
  
  override def toString() = this.railway.fesDesc + " Shift is leading straight: " + this.goingStraight
  
}