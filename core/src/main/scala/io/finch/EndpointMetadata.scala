package io.finch

import com.twitter.finagle.http.{Method => FinagleMethod}
import io.finch.Endpoint.Meta

import scala.reflect.ClassTag

// TODO unseal, for custom endpoints
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

  case class BodyParam[T](
    override val parameterType: ClassTag[T],
    override val description: Option[String] = None,
    override val required: Boolean = true
  ) extends ParameterMetadata[T] {
    override val parameterLocation: String = "body"
    override val parameterNameOpt: Option[String] = Some("body")
  }

  case class EndpointInfo(
    method: Option[FinagleMethod] = None,
    path: Option[String] = None,
    params: Vector[ParameterMetadata[_]] = Vector.empty
  )

  private def combinePathOpts(initOpt: Option[String], nextOpt: Option[String]): Option[String] = (
    initOpt.fold(nextOpt.map("/" + _))(initPath => Some(
      nextOpt.fold(initPath)(initPath + "/" + _)
    ))
    )

  // not @tailrec, see AndThen case
  def consolidateEndpointMeta(meta: EndpointMetadata, inits: Seq[EndpointInfo] = Vector(EndpointInfo())): Seq[EndpointInfo] = meta match {
    // TODO remove unimplemented, and default an endpoint to implement its meta as a NoOp
    case EndpointMetadata.NoOp(num) => println(s"NoOp numbered: $num"); ???
    case EndpointMetadata.Method(method, em) => consolidateEndpointMeta(em, inits.map(init =>  init.copy(
      method = Some(init.method.fold(method)(currentMethod => {
        if (currentMethod != method) {
          // TODO Invalid response for method mismatch
          ???
        } else method
      }))
    )))
    case EndpointMetadata.Path(pathOpt) => inits.map(init => init.copy(path = combinePathOpts(init.path, pathOpt)))
    // TODO this breaks tail recursion :(
    case EndpointMetadata.AndThen(firstMeta, secondMeta) => consolidateEndpointMeta(secondMeta, consolidateEndpointMeta(firstMeta, inits))
    case paramMeta @ EndpointMetadata.PathParam(pathType, pathVarNameOpt, _) => inits.map(init => init.copy(
      // TODO determine random string for an unannotated path, coordinating it with the parameter
      path = combinePathOpts(init.path, Some("{" + pathVarNameOpt.getOrElse("some" + pathType.runtimeClass.getSimpleName) + "}")),
      params = init.params :+ paramMeta
    ))
    case headerParam: EndpointMetadata.HeaderParam[_] => inits.map(init => init.copy(
      params = init.params :+ headerParam
    ))
    // TODO this breaks tail recursion :(
    case EndpointMetadata.MetaList(metas) => metas.flatMap(consolidateEndpointMeta(_, inits))
  }

}