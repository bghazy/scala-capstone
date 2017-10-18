package observatory

object Main extends App {

  val temp_2013 = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv"))
  temp_2013.foreach(v=>println(v))

}
