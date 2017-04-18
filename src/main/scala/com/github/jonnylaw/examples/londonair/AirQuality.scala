package com.github.jonnylaw.examples.londonair

import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import com.github.jonnylaw.model._
import com.github.nscala_time.time.Imports._
import java.sql.Timestamp
import slick.jdbc.SQLiteProfile.api._

/**
  * Json marshalling for Traffic Flow
  */
object LondonAirSensors extends SprayJsonSupport with DefaultJsonProtocol {
  case class Sensor(name: String, data: SensorData)
  case class SensorData(trafficFlow: TrafficFlow)
  case class TrafficFlow(meta: Meta, data: Map[DateTime, Double])
  case class Meta(name: String, theme: String, units: String)

  implicit def dateTimeJsonFormat = new RootJsonFormat[DateTime] {
    val formatter = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")

    def write(obj: DateTime): JsValue = {
      JsString(formatter.print(obj))
    }

    def read(json: JsValue): DateTime = json match {
      case JsString(s) => formatter.parseDateTime(s)
      case _ => deserializationError("DateTime expected")
    }
  }

  implicit val metaFormat =  jsonFormat3(Meta.apply)
  implicit val trafficFlowFormat = jsonFormat2(TrafficFlow.apply)
  implicit val sensorDataFormat: RootJsonFormat[SensorData] = jsonFormat(SensorData.apply, "Traffic Flow")
  implicit val sensorFormat = jsonFormat2(Sensor.apply)
}

/**
  * Slick database implementation for Urban Observatory Traffic Flow Data
  */
object LondonAirDatabaseTables {
  implicit def dateTime =
    MappedColumnType.base[DateTime, Timestamp](
      dt => new Timestamp(dt.getMillis),
      ts => new DateTime(ts.getTime)
    )

  class SitesTable(tag: Tag) extends Table[(String, Boolean, String, DateTime)](tag, "site") {
    "@DataManager": "King's College London",
    "@DataOwner": "Merton",
    "@DateClosed": "",
    "@DateOpened": "2017-01-27 00:00:00",
    "@Latitude": "51.40162",
    "@LatitudeWGS84": "6692543.79001",
    "@LocalAuthorityCode": "24",
    "@LocalAuthorityName": "Merton",
    "@Longitude": "-0.19589212",
    "@LongitudeWGS84": "-21810.7165116",
    "@SiteCode": "ME9",
    "@SiteLink": "http://www.londonair.org.uk/london/asp/publicdetails.asp?site=ME9",
    "@SiteName": "Merton - Morden Civic Centre 2",
    "@SiteType": "Roadside"

    def sensorName = column[String]("sensor_name", O.PrimaryKey)
    def isActive = column[Boolean]("is_active")
    def units = column[String]("units")
    def lastReading = column[DateTime]("last_reading")
    // Every table needs a * projection with the same type as the table's type parameter
    def * = (sensorName, isActive, units, lastReading)
  }
  val sensors = TableQuery[SensorTable]

  class AirQualityTable(tag: Tag) extends Table[(String, DateTime, Double)](tag, "airquality") {
    def sensorName = column[String]("sensor_name")
    def timestamp = column[DateTime]("timestamp")
    def reading = column[Double]("reading")

    def * = (sensorName, timestamp, reading)
    // foreign key referencing the sensors table
    def sensorfk = foreignKey("sensor_name_fk", sensorName, sensors)(_.sensorName)
    // composite primary key on sensor Id and datetime
    def pk = primaryKey("pk_id_datetime", (sensorName, timestamp))
  }

  val trafficFlow = TableQuery[TrafficFlowTable]
}
