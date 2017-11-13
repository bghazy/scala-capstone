package observatory

import java.awt.image.BufferedImage
import java.lang.Math._
import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location =  Location(
    toDegrees(atan(sinh(PI * (1.0 - 2.0 * tile.y / (1 << tile.zoom))))),
    tile.x / (1 << tile.zoom) * 360.0 - 180.0)

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val width = 256
    val height = 256
    val alpha = 127
    val yStart = tile.y * height
    val xStart = tile.x * width

    val stream = (for {
      k <- yStart until yStart + height
      j <- xStart until xStart + width
    } yield (j, k)).toStream.par

    val pixels = stream
      .map({
        case (j, k) => tileLocation(Tile(j, k, tile.zoom))
      })
      .map(predictTemperature(temperatures, _))
      .map(interpolateColor(colors, _))
      .map(color => Pixel(color.red, color.green, color.blue, alpha))
      .toArray

    Image(width, height, pixels, BufferedImage.TYPE_INT_ARGB)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Year, Data)],
                           generateImage: (Year, Tile, Data) => Unit
                         ): Unit = {
    val _ = for {
      (year, data) <- yearlyData
      zoom <- 0 to 3
      x <- 0 until 1 << zoom
      y <- 0 until 1 << zoom
    } {
      generateImage(year, Tile(x, y, zoom), data)
    }
  }

}
