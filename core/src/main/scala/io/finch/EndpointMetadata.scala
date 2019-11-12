package io.finch

import com.twitter.finagle.http.{Method => FinagleMethod}
import io.finch.Endpoint.Meta

import scala.reflect.ClassTag

sealed trait EndpointMetadata
sealed trait ParameterMetadata[T] extends EndpointMetadata {
  val parameterType: ClassTag[T]
  val parameterNameOpt: Option[String]
}

object EndpointMetadata {
  private def addNonListToList(a: Meta, bs: MetaList): MetaList = {
    a match {
      case _: NoOp => bs
      case _ => MetaList(a +: bs.metas)
    }
  }
  def list(a: Meta, b: Meta): MetaList = (a, b) match {
    case (as: MetaList, bs: MetaList) => MetaList(as.metas ++ bs.metas)
    // case (_: NoOp, _: NoOp) => MetaList(List.empty)
    case (as: MetaList, b) => addNonListToList(as.metas.head, MetaList(as.metas.tail :+ b))
    // case (a, _: NoOp) => MetaList(List(a))
    case (a, bs: MetaList) => addNonListToList(a, bs)
    // case (_: NoOp, b) => MetaList(List(b))
    case (a, b) => MetaList(List(a,b))
  }

  case class NoOp(i: Int) extends EndpointMetadata
  case class Method(method: FinagleMethod, em: EndpointMetadata) extends EndpointMetadata
  case class MetaList(metas: Seq[Meta]) extends EndpointMetadata
  case class Path(pathOpt: Option[String]) extends EndpointMetadata
  case class AndThen(firstMeta: Meta, secondMeta: Meta) extends EndpointMetadata
  case class PathParam[T](parameterType: ClassTag[T], parameterNameOpt: Option[String] = None) extends ParameterMetadata[T]

}