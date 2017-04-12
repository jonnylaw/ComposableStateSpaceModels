package com.github.jonnylaw.examples.urbanobservatory

import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import com.github.jonnylaw.model._
import com.github.nscala_time.time.Imports._
import java.sql.Timestamp
import slick.jdbc.SQLiteProfile.api._
// import slick.jdbc.H2Profile.api._

/**
  * Json marshalling for Traffic Flow
  */
object UoSensors extends SprayJsonSupport with DefaultJsonProtocol {
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
object TrafficDatabaseTables {
  implicit def dateTime =
    MappedColumnType.base[DateTime, Timestamp](
      dt => new Timestamp(dt.getMillis),
      ts => new DateTime(ts.getTime)
    )

  class SensorTable(tag: Tag) extends Table[(String, Boolean, String, DateTime)](tag, "sensor") {
    def sensorName = column[String]("sensor_name", O.PrimaryKey)
    def isActive = column[Boolean]("is_active")
    def units = column[String]("units")
    def lastReading = column[DateTime]("last_reading")
    // Every table needs a * projection with the same type as the table's type parameter
    def * = (sensorName, isActive, units, lastReading)
  }
  val sensors = TableQuery[SensorTable]

  class TrafficFlowTable(tag: Tag) extends Table[(String, DateTime, Double)](tag, "trafficflow") {
    def sensorName = column[String]("sensor_name")
    def datetime = column[DateTime]("timestamp")
    def reading = column[Double]("reading")

    def * = (sensorName, datetime, reading)
    // foreign key referencing the sensors table
    def sensorfk = foreignKey("sensor_name_fk", sensorName, sensors)(_.sensorName)
    // composite primary key on sensor Id and datetime
    def pk = primaryKey("pk_id_datetime", (sensorName, datetime))
  }

  val trafficFlow = TableQuery[TrafficFlowTable]
}
