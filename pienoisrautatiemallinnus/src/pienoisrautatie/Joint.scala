package pienoisrautatie

import java.awt.geom._
import java.awt.Graphics2D
import java.awt.Color._
import java.awt.Color

class Joint(val owner: Track, val no:Int, var pairedTo: Option[Joint] = None) {
  
  def getTrack = this.owner
  
  def location = this.owner.connectionPoints.apply(this.no - 1)
  
  def jointRotation = this.owner.jointAngles.apply(this.no - 1)
  
  def rectangle = new Rectangle2D.Double(this.location._1 - 10, this.location._2 - 10, 20, 20)
  
  def enoughSpaceRectangle = new Rectangle2D.Double(this.location._1 - 20, this.location._2 - 20, 40, 40)
  
  //If there is enough space, when joint is disconnected
  
  def enoughSpace: Boolean = {
    if (pairedTo != None) {
      true
    } else {
      
      this.owner.railway.pieces.filterNot(x => x.equals(this.owner)).filter(x => new Area(x.getArea).intersects(this.enoughSpaceRectangle)).isEmpty
    }
  }
  
  def draw(g: Graphics2D) = {
    if (this.owner.isSelected) {
      val g2d= g
      g2d.setColor(Color.white)
      g2d.draw(this.rectangle)
      g2d.setColor(red)
      g2d.draw(this.enoughSpaceRectangle)
    }
  }
  
  override def toString() = "Joint number " + no + " located at " + this.location + " " +  this.jointRotation + " " + (this.pairedTo != None)
}