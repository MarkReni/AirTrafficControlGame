package UI
import Data._
import java.awt.ComponentOrientation
import javax.swing.table.DefaultTableModel
import scala.swing._


case class UI_Timetable(defaultTableModel: DefaultTableModel = new DefaultTableModel(0, 0)) extends Table(defaultTableModel) {
    tooltip = "Timetable window displays information about arrival and departure times."
    val width = GameConstants.timetableScroll_width
    val height = GameConstants.timetableScroll_height + 400
    preferredSize = new Dimension(width, height)
    maximumSize = new Dimension(width, height)
    background = GameConstants.timetableBackground_color
    font = GameConstants.timetable_font
    rowHeight = GameConstants.timetableRow_height
    componentOrientation = ComponentOrientation.LEFT_TO_RIGHT
    autoResizeMode = Table.AutoResizeMode.Off   // makes horizontal scrollbar visible


}
