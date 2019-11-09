package io.finch

import com.twitter.finagle.http.{Method => FinagleMethod}

sealed trait EndpointMetadata

object EndpointMetadata {
  case object NoOp extends EndpointMetadata
  case class Method(method: FinagleMethod, em: EndpointMetadata) extends EndpointMetadata
}