package io.megl.models

import io.circe.derivation.annotations.JsonCodec
import io.circe.derivation.annotations.JsonKey
import io.circe.derivation.annotations.Configuration
import io.circe.Decoder
import io.circe.Encoder
import io.circe.syntax._

@JsonCodec
final case class Root(
    @JsonKey("_info") info: Info,
    endpoints: List[EndPoint],
    types: List[ESType]
)

@JsonCodec
final case class License(name: String, url: String)

@JsonCodec
final case class Info(license: License, title: String, version: String)

@JsonCodec
final case class EndPoint(
    description: String,
    docUrl: String,
    name: String,
    request: Option[ClassReference]=None,
    requestBodyRequired: Boolean = false,
    response: Option[ClassReference]=None,
    since: Option[String]=None,
    stability: Option[String]=None,
    urls: List[EndpointUrl]=Nil,
    accept: List[String]=Nil,
    contentType: List[String]=Nil,
)

@JsonCodec
final case class ClassReference(name: String, namespace: String)

@JsonCodec
final case class Deprecation(description: String, version: String)

@JsonCodec
final case class EndpointUrl(
    methods: List[String],
    path: String,
    deprecation: Option[Deprecation] = None
)

// ESType

@JsonCodec
final case class ESType(
    inherits: List[Inherit]=Nil,
    kind: String,
    name: ClassReference,
    properties: List[Property]=Nil
)

@JsonCodec
final case class Inherit(`type`: ClassReference)

@JsonCodec
final case class Property(name: String, required: Boolean, `type`: TypeProperty)

sealed trait TypeProperty

object TypeProperty {
  implicit val typePropertyDecoder:Decoder[TypeProperty]=Decoder.instance[TypeProperty]{
    cursor =>
      cursor.get[String]("kind") match {
        case Left(value) =>
          Left(value)
        case Right(value) =>
          value match {
            case "instance_of" => cursor.as[InstanceType]
            case "dictionary_of" => cursor.as[DictionaryType]
            case "union_of" => cursor.as[UnionType]
            case "array_of" => cursor.as[ArrayType]
            case "literal_value" => cursor.as[LiteralType]
            case "named_value_of" => cursor.as[NamedValueType]
            case "user_defined_value" => cursor.as[UserDefinedValueType]
          }
      }
  }

  implicit val typePropertyEncoder:Encoder[TypeProperty]=Encoder.instance[TypeProperty]{
    obj =>
      obj match {
        case t:InstanceType => t.asJson
        case t:DictionaryType =>t.asJson
        case t:UnionType =>t.asJson
        case t:ArrayType => t.asJson
        case t:LiteralType => t.asJson
        case t:NamedValueType => t.asJson
        case t:UserDefinedValueType =>t.asJson
      }
  }


}


@JsonCodec
final case class InstanceType(`type`: ClassReference, generics:List[TypeProperty]=Nil) extends TypeProperty

@JsonCodec
final case class DictionaryType(key: TypeProperty, value: TypeProperty)
    extends TypeProperty

@JsonCodec
final case class UnionType(items: List[TypeProperty]=Nil) extends TypeProperty

@JsonCodec
final case class NamedValueType(items: List[TypeProperty]=Nil) extends TypeProperty

@JsonCodec
final case class ArrayType(value: TypeProperty) extends TypeProperty

@JsonCodec
final case class LiteralType(value: String) extends TypeProperty



@JsonCodec
final case class UserDefinedValueType() extends TypeProperty
