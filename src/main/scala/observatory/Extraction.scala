package observatory

import java.time.LocalDate
import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val temperatures = parseFile(temperaturesFile).flatMap(parseTempsLine)
    val stations: Map[StationKey, Location] = parseFile(stationsFile).flatMap(parseStationsLine).toMap


    def toCelsius(f: Double): Double = (f - 32.0) / 1.8

    temperatures.filter(rec => stations.contains(rec.key)).map(
      rec => (LocalDate.of(year, rec.month, rec.day), stations(rec.key), toCelsius(rec.temp))
    ).toSeq
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    def avg(xs: List[Double]) = xs.sum / xs.length

    records.groupBy(_._2).par.map {
      record => (record._1, avg(record._2.map(_._3).toList))
    }.toList
  }

  def parseFile(fileName: String): Iterator[String] = {
    Source.fromInputStream(getClass.getResourceAsStream(fileName)).getLines()
  }

  /**
    * Any Non-numeric input results in the key's component  being None
    *
    * @param stnStr  STN number as string or the empty string
    * @param wbanStr WBAN number as string or the empty string
    * @return StationKey
    */
  def parseStationKey(stnStr: String, wbanStr: String): StationKey =
    StationKey(toInt(stnStr), toInt(wbanStr))

  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: Exception => None
    }
  }


  /**
    * Parse a line from a temperatures file
    *
    * @param str String of STN,WBAN,MONTH,DAY,TEMP
    * @return Parsed line
    */
  def parseTempsLine(str: String): Option[TempsLine] = {
    str.split(",") match {
      case Array(stn, wban, month, day, temp) =>
        Option(TempsLine(parseStationKey(stn, wban), month.toInt, day.toInt, temp.toDouble))
      case _ => None
    }
  }

  def parseStationsLine(str: String): Option[(StationKey, Location)] = {
    str.split(",") match {
      case Array(stn, wban, lat, lon) => {
        if (lat.length > 0 && lon.length > 0) Option((parseStationKey(stn, wban), Location(lat.toDouble, lon.toDouble)))
        else None
      }
      case _ => None
    }
  }
}
