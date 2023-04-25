package Data
import java.awt._


object GameConstants {
  // changes aircraft creation frequency according to the level parameters
  def changeLevel(value: Int): Unit = {
    this.aircraftDepartureMaxFrequency = this.aircraftDepartureMaxFrequencyConst - value
    this.aircraftDepartureMaxFrequencyNonEmpty = this.aircraftDepartureMaxFrequencyNonEmptyConst - value
    this.aircraftArrivalMaxFrequency = this.aircraftArrivalMaxFrequencyConst - value
    this.aircraftArrivalMaxFrequencyNonEmpty = this.aircraftArrivalMaxFrequencyNonEmptyConst - value
    this.aircraftDepartureMinFrequency = (this.aircraftDepartureMinFrequencyConst - (value / 4))
    this.aircraftDepartureMinFrequencyNonEmpty = (this.aircraftDepartureMinFrequencyNonEmptyConst - (value / 4))
    this.aircraftArrivalMinFrequency = (this.aircraftArrivalMinFrequencyConst - (value / 4))
    this.aircraftArrivalMinFrequencyNonEmpty = (this.aircraftArrivalMinFrequencyNonEmptyConst - (value / 4))
  }

  // GENERAL parameters
  final val gameSpeedAirportComponent: Int = 1400 // 1500
  final val gameSpeedRadarComponent: Int = 2200 // 2500
  final val gameSpeedClock: Int = 16 // 16
  final val gameSpeedWeatherStart: Int = 12000 // 12000
  final val gameSpeedWeatherEnd: Int = 6000 // 6000
  final val timeToLiveMax: Int = 6000 // 6000 at start; how fast until airplane gets destroyed due to fuel
  final val aircraftDepartureMinFrequencyConst: Int = 30 // 10; min mins at which aircraft are ready for departure
  final val aircraftDepartureMaxFrequencyConst: Int = this.aircraftDepartureMinFrequencyConst + 20 // 30; max mins at which aircraft are ready for departure
  final val aircraftDepartureMinFrequencyNonEmptyConst: Int = this.aircraftDepartureMaxFrequencyConst + 30 // 50; min mins at which aircraft are ready for departure when gates are non empty
  final val aircraftDepartureMaxFrequencyNonEmptyConst: Int = this.aircraftDepartureMinFrequencyNonEmptyConst + 20// 70; max mins at which aircraft are ready for departure when gates are non empty
  final val aircraftArrivalMinFrequencyConst: Int = 10  // 30; min mins at which aircraft are arriving
  final val aircraftArrivalMaxFrequencyConst: Int = this.aircraftArrivalMinFrequencyConst + 10 // 60; max mins at which aircraft are arriving
  final val aircraftArrivalMinFrequencyNonEmptyConst: Int = this.aircraftArrivalMaxFrequencyConst + 10 // 30; min mins at which aircraft are arriving
  final val aircraftArrivalMaxFrequencyNonEmptyConst: Int = this.aircraftArrivalMinFrequencyNonEmptyConst + 30 // 60; max mins at which aircraft are arriving
  var aircraftDepartureMinFrequency: Int = 30 // 10; min mins at which aircraft are ready for departure
  var aircraftDepartureMaxFrequency: Int = this.aircraftDepartureMinFrequency + 20 // 30; max mins at which aircraft are ready for departure
  var aircraftDepartureMinFrequencyNonEmpty: Int = this.aircraftDepartureMaxFrequency + 30 // 50; min mins at which aircraft are ready for departure when gates are non empty
  var aircraftDepartureMaxFrequencyNonEmpty: Int = this.aircraftDepartureMinFrequencyNonEmpty + 20// 70; max mins at which aircraft are ready for departure when gates are non empty
  var aircraftArrivalMinFrequency: Int = 10  // 30; min mins at which aircraft are arriving OK
  var aircraftArrivalMaxFrequency: Int = this.aircraftArrivalMinFrequency + 10 // 60; max mins at which aircraft are arriving
  var aircraftArrivalMinFrequencyNonEmpty: Int = this.aircraftArrivalMaxFrequency + 10 // 30; min mins at which aircraft are arriving
  var aircraftArrivalMaxFrequencyNonEmpty: Int = this.aircraftArrivalMinFrequencyNonEmpty + 30 // 60; max mins at which aircraft are arriving
  final val delayComponent: Int = 4 // the game will be delayed at this component's rate
  final val title_font_1: Font = new Font("Arial", Font.PLAIN, 30)
  final val title_font_2: Font = new Font("Arial", Font.PLAIN, 25)
  final val title_font_3: Font = new Font("Arial", Font.PLAIN, 20)
  final val background_color: Color = new Color(128, 105, 155)

  // UI ELEMENTS
  /// UI_GameStartWindow
  final val gameStartWindow_width: Int = 2500 // 2500
  final val gameStartWindow_height: Int = 500 // 500
  final val UI_Start_image_width: Int = 1000
  final val UI_Start_image_height: Int = 500

  /// UI_GameOpeningWindow
  final val airport_pic_width: Int = 200
  final val airport_pic_height: Int = 100
  final val buttons_separator_width: Int = 200
  final val buttons_separator_height: Int = 100

  /// UI_GameMainWindow
  final val gameMainWindow_width: Int = 1300 // 1300
  final val gameMainWindow_height: Int = 850  // 850

  /// airport component
  final val airport_X_size: Int = 42  // 42
  final val airport_Y_size: Int = 26  // 26
  final val airportWindow_width: Int = 640 // 640
  final val airportWindow_height: Int = 425 // 425
  final val airportWindow_square_width: Int = 10
  final val airportWindow_square_height: Int = 10
  final val airportWindow_square_border: Int = 5
  final val airport_color: Color = new Color(100, 130, 50)
  final val gate_font: Font = new Font("Arial", Font.PLAIN, 8)
  final val gate_font_text: Font = new Font("Arial", Font.PLAIN, 7)
  final val gate_color: Color = Color.black
  final val gate_text_color: Color = Color.white
  final val runway_font: Font = new Font("Arial", Font.PLAIN, 12)
  final val runway_font_closed: Font = new Font("Arial", Font.PLAIN, 16)
  final val runway_font_text: Font = new Font("Arial", Font.PLAIN, 7)
  final val runway_text_color: Color = Color.white
  final val runway_text_color_closed: Color = Color.red
  final val airportScaledPicSize: Int = 40

  /// radar component
  final val radarScaled_size: Int = 400
  final val radarWindow_width: Int = 400
  final val radarWindow_height: Int = 450
  final val radarWindow_square_width: Int = 6   // 6 (10 if not scaled pic)
  final val radarWindow_square_height: Int = 6  // 6 (10 if not scaled pic)
  final val radarWindow_square_border: Int = 3  // 3 (5 if not scaled pic)
  final val radar_X_start: Int = -1 // -1
  final val radar_Y_start: Int = 0  // 0
  final val radar_X_end: Int = 44 // 44
  final val radar_Y_end: Int = 0  // 0
  final val timeToFlyAcrossRadar: Int = 120 //time that takes for the airplane to fly across radar 120 minutes
  final val radarScaledPicSize: Int = 30

  /// timetable component
  final val timetableScroll_width: Int = 320  // 320
  final val timetableScroll_height: Int = 425 // 425
  final val timetable_font: Font = new Font("New Times Roman", Font.PLAIN, 14)
  final val timetableBackground_color: Color = new Color(255, 255, 255)
  final val timetableRow_height: Int = 40

  /// information panel
  final val buttons_width: Int = 600
  final val buttons_height: Int = 30
  final val statisticsLabel_width: Int = 400
  final val statisticsLabel_height: Int = 30
  final val informationPanel_width: Int = 230
  final val informationPanel_height: Int = 425


}
