package pienoisrautatie

import java.awt.Graphics2D
import scala.math._
import scala.collection.mutable.Buffer
import java.awt.Shape
import java.awt.geom.Area

abstract class Track(val railway:Railway) {
  
  var rotation: Int = 0
  
  //Tracks position
  
  var centerLocationX: Int = 500
  var centerLocationY: Int = 500
  
  //Contains 1-4 joint(s)
  
  var joints = List[Joint]()
  
  //If there is enough space for every disconnected joint
  
  def haveEnoughSpace = this.joints.filterNot(_.enoughSpace).isEmpty
  
  def jointAngles: List[Int]
  
  //Returns the area that is covered by this track
  
  def getArea: Shape
  
  def changeAngle = {val a = 1}
  
  def changeLength(e: Int) = {val a = 1}
  
  def makeShift = {val a = 1}
  
  def getConnectionRects = this.joints.map(_.rectangle)
  
  def jointsOfOtherTracks  = this.railway.pieces.map(_.joints).filter(!_.equals(this.joints)).flatten
  
  //Used for saving
  
  def trackToList: List[String]
  
  //Checks loops and connections
  
  def checkStatus = {
    this.railway.pieces.foreach(_.connect())
    this.railway.checkLoops
  }
  
  //Makes a connection to an other track
  
  def connect() = {

    val jointsOfOtherTracks  = this.railway.pieces.map(_.joints).filter(!_.equals(this.joints)).flatten
    for (i <- 0 until this.joints.length) {
      val found = jointsOfOtherTracks.filter(_.rectangle.intersects(this.joints.apply(i).rectangle)).filterNot(_.owner.overlaps)
      if (found.length > 0 && found.head.jointRotation % 180 == this.joints.apply(i).jointRotation % 180) {
        this.joints.apply(i).pairedTo = Some(found.head)
      } else {
    			this.joints.apply(i).pairedTo = None
  	  }
    }
  }
  
  def isSelected: Boolean = {
    if (this.railway.selected.isDefined) {
      this == this.railway.selected.get
    } else false
  }
  
  //Used for setting tracks positin when dragging with mouse or loading from file
  
  def setPosition(x: Int, y: Int) = {
    
    
    if (this.isSelected) {
      this.centerLocationX = ((x/5)*5).toInt + this.railway.clickOffset._1
      this.centerLocationY = ((y/5)*5).toInt + this.railway.clickOffset._2
    } else {
    this.centerLocationX = ((x/5)*5).toInt
    this.centerLocationY = ((y/5)*5).toInt
    }
    this.checkStatus
    this.railway.setCommentary("Piece moved to " + this.centerLocationX + "," + this.centerLocationY)
  }
 
  def setRotation(rot: Int) = {
    this.checkStatus
    this.rotation = rot % 360
  }
  
  def connectionPoints: List[(Int, Int)]
      
  def rotate() = {
    if (this.rotation == 355) {this.rotation = 0} else {this.rotation += 5}
    this.railway.setCommentary("Rotated clockwise")
    this.checkStatus
  }
  
  def inverseRotate() = {
    if (this.rotation == 0) {this.rotation = 355} else {this.rotation -= 5}
    this.railway.setCommentary("Rotated counter clockwise")
    this.checkStatus
  }
  
  def neighbours = this.joints.map(_.pairedTo).filter(_ != None).map(_.get.owner)
  
  def shapeIntersection(other: Shape): Boolean = {
    val a1 = new Area(this.getArea)
    a1.intersect(new Area(other))
    !a1.isEmpty()
  }
  
  def overlaps = !this.railway.pieces.filterNot(x => x.equals(this)).filter(other => this.shapeIntersection(other.getArea)).isEmpty
  
  def moveUp() = {
    if (this.centerLocationY > 5) {
      this.centerLocationY -= 5
      this.railway.setCommentary("Moved upwards")
      this.checkStatus
    }
  }
  
  def moveDw() = {
    if (this.centerLocationY < 1200) {
      this.centerLocationY += 5
      this.railway.setCommentary("Moved downwards")
      this.checkStatus
      }
  }
  
  def moveLe() = {
    if (this.centerLocationX > 5) {
      this.centerLocationX -= 5
      this.railway.setCommentary("Moved left")
      this.checkStatus
    }
  }
  
  def moveRi() = {
    if (this.centerLocationX < 1500) {
      this.centerLocationX += 5
      this.railway.setCommentary("Moved right")
      this.checkStatus
    }
  }
  
  def contains(x: Int, y: Int): Boolean = new Area(this.getArea).contains(x, y)
  
  def draw(g: Graphics2D)
    
}