package io.finch

import com.twitter.finagle.http.{Method => FinagleMethod}
import io.finch.Endpoint.Meta

import scala.reflect.ClassTag

sealed trait EndpointMetadata
sealed trait ParameterMetadata[T] extends EndpointMetadata {
  val parameterType: ClassTag[T]
  val parameterNameOpt: Option[String]
  val description: Option[String]
  val parameterLocation: String
  val required: Boolean
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

  case class PathParam[T](
    override val parameterType: ClassTag[T],
    override val parameterNameOpt: Option[String] = None,
    override val description: Option[String] = None
  ) extends ParameterMetadata[T] {
    override val parameterLocation: String = "path"
    override val required: Boolean = true
  }

  case class HeaderParam[T](
    override val parameterType: ClassTag[T],
    override val parameterNameOpt: Option[String] = None,
    override val description: Option[String] = None,
    override val required: Boolean = true
  ) extends ParameterMetadata[T] {
    override val parameterLocation: String = "header"
  }

}