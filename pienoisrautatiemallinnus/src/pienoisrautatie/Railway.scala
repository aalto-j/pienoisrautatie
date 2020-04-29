package pienoisrautatie

import java.awt.Graphics2D
import scala.collection.mutable.Buffer

class Railway {
  
  //All pieces
  
  var pieces = Buffer[Track]()
  
  //Descriptions for different types of track
  
  var straightDesc = "A straight piece of track."
  var curvedDesc = "A curved piece of track."
  var endDesc = "The last piece of the railway."
  var teslDesc = "A three end shift turning left."
  var tesrDesc = "A three end shift turning right."
  var fesDesc = "A four end shift."
  
  var commentaryText = "Welcome"
  
  //How far our click is from the piece center location. Makes dragging pieces nicer
  
  var clickOffset = (0, 0)
  
  def allConnected = this.pieces.map(_.joints).flatten.filter(_.pairedTo == None).isEmpty
  
  //For finding loops
  
  def checkLoops = {
    var piecesPossiblyInLoop = this.pieces.filter(_.neighbours.length == 2)
    var pieces = piecesPossiblyInLoop.filter(x => piecesPossiblyInLoop.filterNot(_.equals(x)).contains(x.neighbours.apply(0)) && piecesPossiblyInLoop.filterNot(_.equals(x)).contains(x.neighbours.apply(1)))
    var piecescopy = pieces
    pieces = pieces.filter(x => pieces.contains(x.neighbours.apply(0)) && pieces.contains(x.neighbours.apply(1)))
    if (pieces.length == piecescopy.length && pieces.length != 0) {this.setCommentary("You created a loop")}
  }
  
  var selected: Option[Track] = None
  
  def descriptionsToList = {
    List[String]("RAILWAY", this.straightDesc, this.curvedDesc, this.endDesc, this.teslDesc, this.tesrDesc, this.fesDesc)
  }
  
  //For reading descriptions from file
  
  def setDescriptions(row: List[String]) = {
    for (i <- 1 until 6) {
      i match {
        case 1 => if (!row.apply(i).isEmpty) {this.straightDesc = row.apply(i)}
        case 2 => if (!row.apply(i).isEmpty) {this.curvedDesc = row.apply(i)}
        case 3 => if (!row.apply(i).isEmpty) {this.endDesc = row.apply(i)}
        case 4 => if (!row.apply(i).isEmpty) {this.teslDesc = row.apply(i)}
        case 5 => if (!row.apply(i).isEmpty) {this.tesrDesc = row.apply(i)}
        case 6 => if (!row.apply(i).isEmpty) {this.fesDesc = row.apply(i)}
      }
    }
  }
  
  def select(x: Int, y: Int) = {
    val piecesInArea = pieces.filter(_.contains(x, y))
    if (!piecesInArea.isEmpty) {
      this.selected = Some(piecesInArea.apply(0))
      this.clickOffset = (this.selected.get.centerLocationX - x, this.selected.get.centerLocationY - y)
      this.setCommentary(this.selected.get.toString())
    } else {
      this.selected = None
      this.setCommentary("Nothing selected")
    }
  }
  
  def setCommentary(t: String) = this.commentaryText = t
  
  def deletePiece(p: Track) = this.pieces -= p
   
  def containsOverlaps = !this.pieces.filter(x => x.overlaps || !x.haveEnoughSpace).isEmpty
  
  //Add methods for different types of track
  
  def addStraight() = pieces = pieces += new StraightTrack(this)
  
  def addStraight(x: Int, y:Int, rot:Int, len: Int) = {
    val piece = new StraightTrack(this)
    piece.setPosition(x, y)
    piece.setRotation(rot)
    piece.setLength(len)
    pieces = pieces += piece
  }
  
  def addCurved() = pieces = pieces += new CurvedTrack(this)
  
  def addCurved(x: Int, y:Int, rot:Int, rad: Int, ang: Boolean) = {
    val piece = new CurvedTrack(this)
    piece.setPosition(x, y)
    piece.setRadius(rad)
    if (ang) {piece.changeAngle}
    piece.setRotation(rot)
    pieces = pieces += piece
  }
  
  def addEnd() = pieces = pieces += new EndTrack(this)
  
  def addEnd(x: Int, y:Int, rot:Int) = {
    val piece = new EndTrack(this)
    piece.setPosition(x, y)
    piece.setRotation(rot)
    pieces = pieces += piece
  }
  
  def addThreeEndShiftL() = pieces = pieces += new ThreeEndShiftL(this)
  
  def addThreeEndShiftL(x: Int, y:Int, rot:Int) = {
    val piece = new ThreeEndShiftL(this)
    piece.setPosition(x, y)
    piece.setRotation(rot)
    pieces = pieces += piece
  }
  
  def addThreeEndShiftR() = pieces = pieces += new ThreeEndShiftR(this)
  
  def addThreeEndShiftR(x: Int, y:Int, rot:Int) = {
    val piece = new ThreeEndShiftR(this)
    piece.setPosition(x, y)
    piece.setRotation(rot)
    pieces = pieces += piece
  }
  
  def addFourEndShift() = pieces = pieces += new FourEndShift(this)
  
  def addFourEndShift(x: Int, y:Int, rot:Int) = {
    val piece = new FourEndShift(this)
    piece.setPosition(x, y)
    piece.setRotation(rot)
    pieces = pieces += piece
  }
  
  def size = pieces.size
  
  def neighborsOfPieces = this.pieces.map(_.neighbours)
  
  
  
  def draw(g: Graphics2D) = {
    pieces.foreach(_.draw(g))
    pieces.map(_.joints).flatten.foreach(_.draw(g))
  }
  
  
  override def toString() = "your railway"
  
}