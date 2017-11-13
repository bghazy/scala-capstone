package observatory

import java.awt.image.BufferedImage
import com.sksamuel.scrimage.{Image, Pixel}
import java.lang.Math._
import scala.annotation.tailrec

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val p = 5.0
  val distanceThreshold = 1000.0
  val earthRadius = 6371000.0

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {

    def distance(a: Location, b: Location): Double = {
      val dLon = toRadians(abs(a.lon - b.lon))
      val aLatR = toRadians(a.lat)
      val bLatR = toRadians(b.lat)
      val radians = acos(sin(aLatR) * sin(bLatR) + cos(aLatR) * cos(bLatR) * cos(dLon))
      earthRadius * radians
    }

    val weights = temperatures.par.map {
      case (loc, _) => {
        val d = distance(loc, location)
        if (d > distanceThreshold)
          1.0 / pow(d, p)
        else
          1.0
      }
    }
    val sumOfWeights = weights.sum
    val sumOfWeightedTemps = temperatures.par.zip(weights).map {
      case ((loc, temp), weight) =>
        val d = distance(loc, location)
        if (d > distanceThreshold) {
          weight * temp
        } else
          temp
    }.sum

    sumOfWeightedTemps / sumOfWeights
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    def findColor(hi: Int, lo: Int, t: Double): Int = {
      if (hi == lo) hi
      else abs((lo * (1.0 - t) + hi * t + 0.5).toInt)
    }

    def findBounds(points: Iterable[(Temperature, Color)], value: Temperature): ((Temperature, Color), (Temperature, Color)) = {
      @tailrec
      def findBoundsRec(points: Iterable[(Temperature, Color)], value: Temperature,
                        hi: (Temperature, Color), lo: (Temperature, Color)): ((Temperature, Color), (Temperature, Color)) = {
        points match {
          case Nil => (hi, lo)
          case p :: tail => {
            if (p._1 == value) (p, p)
            else if (p._1 > value) (hi, p)
            else findBoundsRec(tail, value, p, p)
          }
        }
      }
      findBoundsRec(points, value, points.head, points.head)
    }

    val (hi, lo) = findBounds(points.toArray.sortBy(_._1).toList, value)
    if (hi == lo)
      hi._2
    else {
      val t = Math.abs((value - lo._1) / (hi._1 - lo._1))
      Color(
        findColor(hi._2.red, lo._2.red, t),
        findColor(hi._2.green, lo._2.green, t),
        findColor(hi._2.blue, lo._2.blue, t))

    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)],
                colors: Iterable[(Double, Color)]): Image = {

    val imgType = BufferedImage.TYPE_INT_RGB
    val width = 360
    val height = 180

    def findIndexToLocation(i: Int, rowWidth: Int): Location = {
      val rowIndex = i / rowWidth
      val colIndex = i - (rowIndex * rowWidth)
      val lat = if (rowIndex <= 90) 90 - rowIndex else (rowIndex - 90) * -1
      val lon = if (colIndex <= 180) colIndex - 180 else colIndex - 180
      Location(lat, lon)
    }

    val pixels = Stream.range(0, width * height).par
      .map(findIndexToLocation(_, width))
      .map(predictTemperature(temperatures, _))
      .map(interpolateColor(colors, _))
      .map(color => Pixel(color.red, color.green, color.blue, 255))
      .toArray

    Image(width, height, pixels, imgType)
  }
}

