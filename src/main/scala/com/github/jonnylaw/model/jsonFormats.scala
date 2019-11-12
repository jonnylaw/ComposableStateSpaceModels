package com.github.jonnylaw.model

import breeze.linalg.DenseVector
import cats.implicits._
import cats.Show
import spray.json._
import scala.util.Try
import org.joda.time.DateTime
import com.github.nscala_time.time.Imports._
import spray.json._

/**
  * Marshalling from JSON and to JSON for Simulated Data and MCMC data
  * from the Composed Models Package
  */
object DataProtocols extends DefaultJsonProtocol {
  implicit def denseVectorFormat = new RootJsonFormat[DenseVector[Double]] {
    def write(vec: DenseVector[Double]) = JsArray(vec.data.map(_.toJson).toVector)
    def read(value: JsValue) = value match {
      case JsArray(elements) => {
        val s: Array[Double] = elements.map(_.convertTo[Double]).toArray[Double]
        DenseVector(s)
      }
      case JsNumber(a) => DenseVector(a.toDouble)
      case x => deserializationError("Expected DenseVector as JsArray, but got " + x)
    }
  }

  implicit val brownianFormat = jsonFormat3(BrownianParameter.apply)
  implicit val genbrownianFormat = jsonFormat4(GenBrownianParameter.apply)
  implicit val ornFormat = jsonFormat5(OuParameter.apply)

  implicit def sdeParamFormat = new RootJsonFormat[SdeParameter] {
   def write(obj: SdeParameter): JsValue = obj match {
     case b: BrownianParameter => b.toJson
     case gb: GenBrownianParameter => gb.toJson
     case ou: OuParameter => ou.toJson
   }

    def read(value: JsValue): SdeParameter = value match {
      case obj: JsObject if (obj.fields.size == 3) => value.convertTo[BrownianParameter]
      case obj: JsObject if (obj.fields.size == 4) => value.convertTo[GenBrownianParameter]
      case obj: JsObject => value.convertTo[OuParameter]
    }
  }

  implicit val paramNodeFormat = jsonFormat2(ParamNode)
//  implicit val paramLeafFormat = jsonFormat1(Leaf[ParamNode])

  implicit def leafParamFormat = new RootJsonFormat[Leaf[ParamNode]] {
    def write(obj: Leaf[ParamNode]): JsValue = {
      obj.value.toJson
    }

    def read(value: JsValue): Leaf[ParamNode] = {
      Leaf(value.convertTo[ParamNode])
    }
  }

  implicit def paramsFormat = new RootJsonFormat[Parameters] {
    def write(obj: Parameters) = {
      def loop(params: Parameters): Vector[JsValue] = params match {
        case l: Leaf[ParamNode] => Vector(l.toJson)
        case Branch(l, r) => loop(l) ++ loop(r)
        case Empty => Vector()
      }

      JsArray(loop(obj))
    }
    def read(value: JsValue) = value match {
      case JsArray(elements) => {
        elements.
          map(_.convertTo[Leaf[ParamNode]]).
          reduce((a: Parameters, b: Parameters) => a |+| b)
      }
      case x => deserializationError("Expected Some Parameters, but got " + x)
    }
  }

  implicit val branchParamFormat: JsonFormat[Branch[ParamNode]] = lazyFormat(jsonFormat2(Branch[ParamNode]))

  implicit val leafFormat = jsonFormat1(Leaf[DenseVector[Double]])

  implicit val stateFormat = new RootJsonFormat[State] {
    def write(obj: State) = {
      def loop(state: State): Vector[JsValue] = state match {
        case l: Leaf[DenseVector[Double]] => Vector(l.toJson)
        case Branch(l, r)                 => loop(l) ++ loop(r)
        case Empty                        => Vector()
      }

      JsArray(loop(obj))
    }
    def read(value: JsValue) = value match {
      case JsArray(elements) => {
        elements.
          map(_.convertTo[Leaf[DenseVector[Double]]]).
          reduce((a: State, b: State) => a |+| b)
      }
      case x => deserializationError("Expected Some states, but got " + x)
    }
  }

  implicit val branchFormat: JsonFormat[Branch[DenseVector[Double]]] = lazyFormat(jsonFormat2(Branch[DenseVector[Double]]))

  implicit def dateTimeJsonFormat = new RootJsonFormat[DateTime] {
    val dateTimeFormat = new DateTime()
    val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZZ")

    def write(obj: DateTime): JsValue = {
      JsString(formatter.print(obj))
    }

    def read(json: JsValue): DateTime = json match {
      case JsString(s) => formatter.parseDateTime(s)
      case _ => deserializationError("DateTime expected")
    }
  }

  implicit def stateSpaceFormat[S: JsonFormat] = jsonFormat2(StateSpace.apply[S])
  implicit def metropFormat[P: JsonFormat, S: JsonFormat] = jsonFormat4(MetropState.apply[P, S])
  implicit def pmmhFormat[P: JsonFormat] = jsonFormat3(ParamsState.apply[P])
  implicit val tdFormat = jsonFormat2(TimedObservation.apply)
  implicit val osFormat = jsonFormat5(ObservationWithState.apply)
  implicit val tsFormat = jsonFormat3(TimestampObservation.apply)
  implicit val decompFormat = jsonFormat5(DecomposedModel.apply)

  implicit def dataFormat = new RootJsonFormat[Data] {
   def write(obj: Data): JsValue = obj match {
     case t: TimedObservation => t.toJson
     case o: ObservationWithState => o.toJson
     case ts: TimestampObservation => ts.toJson
     case _ => deserializationError("Data object expected")
   }
    def read(value: JsValue) = Try(value.convertTo[TimedObservation]).
      getOrElse(value.convertTo[ObservationWithState])
  }

  implicit val intFormat = jsonFormat2(CredibleInterval.apply)

  implicit def pfOutFormat[P: JsonFormat] = jsonFormat6(PfOut.apply[P])

  implicit def pfStateFormat[P: JsonFormat] = jsonFormat5(PfState.apply[P])
}

object JsonFormatShow {
  import DataProtocols._

  implicit def stateShow = new Show[State] {
    def show(a: State): String = a.toJson.compactPrint
  }

  implicit def dataShow(implicit S: Show[State]) = new Show[Data] {
    def show(a: Data): String = a.toJson.compactPrint
  }

  implicit def decompShow = new Show[DecomposedModel] {
    def show(a: DecomposedModel): String = a.toJson.compactPrint
  }

  implicit def dvShow = new Show[DenseVector[Double]] {
    def show(dv: DenseVector[Double]) = dv.toJson.compactPrint
  }

  implicit def sdeParamShow = new Show[SdeParameter] {
    def show(p: SdeParameter): String = p.toJson.compactPrint
  }

  implicit def parameterShow = new Show[Parameters] {
    def show(p: Parameters): String = p.toJson.compactPrint
  }

  implicit def stateSpaceShow = new Show[StateSpace[State]] {
    def show(a: StateSpace[State]): String = a.toJson.compactPrint
  }

  implicit def itersShow = new Show[MetropState[Parameters, State]] {
    def show(a: MetropState[Parameters, State]): String = a.toJson.compactPrint
  }

  implicit def paramStateShow(implicit S: Show[Parameters]) = new Show[ParamsState[Parameters]] {
    def show(a: ParamsState[Parameters]): String = a.toJson.compactPrint
  }

  implicit def filterShow = new Show[PfState[State]] {
    def show(a: PfState[State]): String = a.toJson.compactPrint
  }

  implicit def credibleIntervalsShow = new Show[CredibleInterval] {
    def show(a: CredibleInterval): String = a.toJson.compactPrint
  }

  implicit def filterOutShow = new Show[PfOut[State]] {
    def show(a: PfOut[State]): String = a.toJson.compactPrint
  }
}
