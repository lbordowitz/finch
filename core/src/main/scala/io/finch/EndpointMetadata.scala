package io.finch

sealed trait EndpointMetadata

object EndpointMetadata {
  case object NoOp extends EndpointMetadata
}