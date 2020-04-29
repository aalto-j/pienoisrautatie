package pienoisrautatie

import scala.swing._
import scala.collection.mutable._
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal
import scala.math.{sin, cos, toRadians}
import pienoisrautatie.constants._
import java.awt.geom.{GeneralPath, Path2D, Arc2D, AffineTransform, Area}
import java.awt.event._
import javax.swing.{TransferHandler, JLabel}
import java.awt.image.BufferedImage
import scala.swing.event.KeyPressed
import scala.swing.event.Key
import java.awt.event.KeyListener
import scala.io.{Source}
import java.io.{BufferedWriter, FileWriter, IOException}
import javax.swing.ImageIcon

class CorruptedFileException(message: String) extends Exception(message)

class FileNotFoundException(message: String) extends Exception(message)

object RailwayApp extends SimpleSwingApplication {
  
  def writeFile(filename: String, rw: Railway): Unit = {
    if (!rw.containsOverlaps) {
      try {
        val writer = new FileWriter("files/railway.csv")
        val lines = rw.pieces.map(_.trackToList)
        writer.append(rw.descriptionsToList.mkString(","))
        writer.append("\n")
        for (i <- 0 until lines.length) {
          writer.append(lines.apply(i).mkString(","))
          writer.append("\n")
        }
        writer.flush()
        writer.close()
        if (!rw.allConnected) {Dialog.showMessage(top, "Your railway doesn't work until you connect all of the ends of all tracks. Despite this your work is saved", "Work saved")}
        
      } catch {
        case e:IOException =>
          val railwayExc = new CorruptedFileException("Writing railway data failed.")
          railwayExc.initCause(e)
          throw railwayExc
      }
    } else {Dialog.showMessage(top, "Your railway contains overlapping pieces. Work can't be saved.", "Can't be saved")}
  }
  
  
  def readFile(filename: String): Railway = {
    try {
      val src = Source.fromFile("files/railway.csv")
      val newRailway = new Railway
      val rows = ArrayBuffer[List[String]]()
      for (line <- src.getLines()) { rows += line.split(",").map(_.trim()).toList }
      src.close()
      if (!rows.head.head.equals("RAILWAY")) {
        throw new CorruptedFileException("This type of file is not valid.")
      }
      
      for (i <- 0 until rows.length) {
        rows.apply(i).apply(0) match {
          case "RAILWAY"  => newRailway.setDescriptions(rows.apply(i))
          case "S"        => newRailway.addStraight(      rows.apply(i).apply(1).toInt, rows.apply(i).apply(2).toInt, rows.apply(i).apply(3).toInt, rows.apply(i).apply(4).toInt)
          case "3"        => newRailway.addCurved(        rows.apply(i).apply(1).toInt, rows.apply(i).apply(2).toInt, rows.apply(i).apply(3).toInt, rows.apply(i).apply(4).toInt, true)
          case "4"        => newRailway.addCurved(        rows.apply(i).apply(1).toInt, rows.apply(i).apply(2).toInt, rows.apply(i).apply(3).toInt, rows.apply(i).apply(4).toInt, false)
          case "E"        => newRailway.addEnd(           rows.apply(i).apply(1).toInt, rows.apply(i).apply(2).toInt, rows.apply(i).apply(3).toInt)
          case "TL"       => newRailway.addThreeEndShiftL(rows.apply(i).apply(1).toInt, rows.apply(i).apply(2).toInt, rows.apply(i).apply(3).toInt)
          case "TR"       => newRailway.addThreeEndShiftR(rows.apply(i).apply(1).toInt, rows.apply(i).apply(2).toInt, rows.apply(i).apply(3).toInt)
          case "F"        => newRailway.addFourEndShift(  rows.apply(i).apply(1).toInt, rows.apply(i).apply(2).toInt, rows.apply(i).apply(3).toInt)
          case _          => 
        }
      }
      newRailway
    } catch {
      case NonFatal(t) => val a = Dialog.showMessage(top, "Problem with reading file.", "Can't be read")
      new Railway
    }
  }
  
  val width      = 1200
  val height     = 700
  val fullHeight = 1210

  var myRailway = new Railway
  
  def top = new MainFrame {
    title = "Railway"
    
    minimumSize   = new Dimension(200, 210)
    preferredSize = new Dimension(width, fullHeight)
    maximumSize   = new Dimension(1500, 1510)
    
    
    
    this.menuBar = new MenuBar {
      contents += new Menu("File"){
        mnemonic = scala.swing.event.Key.P
        
        contents += new MenuItem(Action("Exit") {System.exit(0)})
      }
      contents += new Menu("Railway"){
        contents += new MenuItem(Action("Move") {/**/})
      }
    }
    
    val buttons = new BoxPanel(Orientation.Vertical){
         
      val importButton = new Button {
        icon = new ImageIcon("images/import.png")
        opaque = false
        contentAreaFilled = false
      }
      
      val saveButton = new Button {
        icon = new ImageIcon("images/save.png")
        opaque = false
        contentAreaFilled = false
      }
      
      val straigthButtonImg, curvedButtonImg = new BufferedImage(64, 64, BufferedImage.TYPE_INT_ARGB)
      private var g2 = straigthButtonImg.createGraphics(); g2.setColor(green)
      g2.fillRect(7, 26, 50, 12)
      
      val straightButton = new Button() {
        icon = new ImageIcon(straigthButtonImg)
        opaque = false
        contentAreaFilled = false
      }
      
      g2 = curvedButtonImg.createGraphics(); g2.setColor(green)
      val aT = new AffineTransform
      aT.rotate(toRadians(15), 32, 32)
      aT.translate((32 - 64 - trackWidth/2 - cos(toRadians(45/2))*64), (32 - 64 - trackWidth/2 + sin(toRadians(45/2))*64))
      val a1 = new Area(new Arc2D.Double(0, 0, 2*64 + trackWidth, 2*64 + trackWidth, 0, 45, Arc2D.PIE))
      val a2 = new Area(new Arc2D.Double(24  , 19 , 2*64 - trackWidth + 10, 2*64 - trackWidth + 10, -5, 60, Arc2D.PIE))
      a1.subtract(a2)
      g2.fill(aT.createTransformedShape(a1))
      
      val curvedButton = new Button() {
        icon = new ImageIcon(curvedButtonImg)
        opaque = false
        contentAreaFilled = false
       }
      
      val endButton = new Button {
        icon = new ImageIcon("images/End100.png")
        opaque = false
        contentAreaFilled = false
      }
      
       val threeEndShiftLeftButton = new Button {
        icon = new ImageIcon("images/Tesl.png")
        opaque = false
        contentAreaFilled = false
      }
      
        val threeEndShiftRightButton = new Button {
        icon = new ImageIcon("images/Tesr.png")
        opaque = false
        contentAreaFilled = false
      }
      
      val fourEndShiftButton = new Button {
        icon = new ImageIcon("images/Fes.png")
        opaque = false
        contentAreaFilled = false
      }
      
      val deleteTrackButton = new Button {
        icon = new ImageIcon("images/deleteTrack.png")
        opaque = false
        contentAreaFilled = false
      }
      
      val deleteButton = new Button {
        icon = new ImageIcon("images/delete.png")
        opaque = false
        contentAreaFilled = false
      }
      
      contents += importButton
      contents += saveButton
      contents += straightButton
      contents += curvedButton
      contents += endButton
      contents += threeEndShiftLeftButton
      contents += threeEndShiftRightButton
      contents += fourEndShiftButton
      contents += deleteTrackButton
      contents += deleteButton
    
    }
    
    val lab = new Label() {text = "Welcome"; foreground = red}
    
    val buildArea = new Panel() {
      
      override def paintComponent(g: Graphics2D) = {
        super.paintComponent(g)
        super.background_=(new Color(180, 180, 180))
        g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)        
        myRailway.draw(g)
      }
    }
    
    
    val arena = new BorderPanel() {
      add(buildArea, BorderPanel.Position.Center)
      add(buttons, BorderPanel.Position.East)
      add(lab, BorderPanel.Position.South)
    }

    contents = arena
    arena.requestFocus
    listenTo(arena.mouse.clicks, arena.keys, arena.mouse.moves, buttons.straightButton, buttons.curvedButton, buttons.endButton, buttons.threeEndShiftLeftButton, buttons.threeEndShiftRightButton, buttons.fourEndShiftButton, buttons.importButton, buttons.saveButton, buttons.deleteButton, buttons.deleteTrackButton)
    
    reactions += {
        
      case scala.swing.event.MousePressed(_, point, _, _, _)  => myRailway.select(point.x, point.y)
      case KeyPressed(_, Key.Up, _, _)                        => if (myRailway.selected != None)   myRailway.selected.get.moveUp()
      case KeyPressed(_, Key.Down, _, _)                      => if (myRailway.selected != None)   myRailway.selected.get.moveDw()
      case KeyPressed(_, Key.Left, _, _)                      => if (myRailway.selected != None)   myRailway.selected.get.moveLe()
      case KeyPressed(_, Key.Right, _, _)                     => if (myRailway.selected != None)   myRailway.selected.get.moveRi()
      case KeyPressed(_, Key.R, _, _)                         => if (myRailway.selected != None)   myRailway.selected.get.rotate()
      case KeyPressed(_, Key.E, _, _)                         => if (myRailway.selected != None)   myRailway.selected.get.inverseRotate()
      case KeyPressed(_, Key.Delete, _, _)                    => if (myRailway.selected != None)   myRailway.deletePiece(myRailway.selected.get)
      case KeyPressed(_, Key.A, _, _)                         => if (myRailway.selected != None && myRailway.selected.get.isInstanceOf[CurvedTrack]) myRailway.selected.get.changeAngle
      case KeyPressed(_, Key.S, _, _)                         => if (myRailway.selected != None && (myRailway.selected.get.isInstanceOf[ThreeEndShift] || myRailway.selected.get.isInstanceOf[FourEndShift])) myRailway.selected.get.makeShift
      case KeyPressed(_, Key.Plus, _, _)                      => if (myRailway.selected != None)   myRailway.selected.get.changeLength(1)
      case KeyPressed(_, Key.Minus, _, _)                     => if (myRailway.selected != None)   myRailway.selected.get.changeLength(-1)
      case scala.swing.event.MouseDragged(_, point, _)        => if (myRailway.selected != None)   myRailway.selected.get.setPosition(point.x, point.y)
      case scala.swing.event.ButtonClicked(buttons.deleteTrackButton)         => if (myRailway.selected != None) myRailway.deletePiece(myRailway.selected.get); arena.requestFocus()
      case scala.swing.event.ButtonClicked(buttons.importButton)              => myRailway = readFile("files/railway.csv"); arena.requestFocus()
      case scala.swing.event.ButtonClicked(buttons.saveButton)                => writeFile("files/railway.csv", myRailway); arena.requestFocus()
      case scala.swing.event.ButtonClicked(buttons.straightButton)            => myRailway.addStraight();             arena.requestFocus()
      case scala.swing.event.ButtonClicked(buttons.curvedButton)              => myRailway.addCurved();               arena.requestFocus()
      case scala.swing.event.ButtonClicked(buttons.endButton)                 => myRailway.addEnd();                  arena.requestFocus()
      case scala.swing.event.ButtonClicked(buttons.threeEndShiftLeftButton)   => myRailway.addThreeEndShiftL();       arena.requestFocus()
      case scala.swing.event.ButtonClicked(buttons.threeEndShiftRightButton)  => myRailway.addThreeEndShiftR();       arena.requestFocus()
      case scala.swing.event.ButtonClicked(buttons.fourEndShiftButton)        => myRailway.addFourEndShift();         arena.requestFocus()
      case scala.swing.event.ButtonClicked(buttons.deleteButton)              => myRailway = new Railway;             arena.requestFocus()
    }
    
    
    val listener = new java.awt.event.ActionListener(){
      def actionPerformed(e: java.awt.event.ActionEvent) = {
        lab.text = myRailway.commentaryText
        arena.repaint()
        

      }
    }
    
    val timer = new javax.swing.Timer(6, listener)
    timer.start()
    
  }
  
}