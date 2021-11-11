# Autodocodec

Autodocodec is short for "self(auto)- documenting encoder and decoder".

In short:
You write a single instance, of the 'Codec' type-class, for your type, and you get:

* [A 'ToJSON' instance from 'aeson'](https://hackage.haskell.org/package/aeson-2.0.1.0/docs/Data-Aeson-Types.html#t:ToJSON)
* [A 'FromJSON' instance from 'aeson'](https://hackage.haskell.org/package/aeson-2.0.1.0/docs/Data-Aeson-Types.html#t:FromJSON)
* [A 'ToYaml] instance from 'yaml'](https://hackage.haskell.org/package/yaml-0.11.7.0/docs/Data-Yaml-Builder.html#t:ToYaml)
* [A json schema](http://json-schema.org/)
* [A nicely-coloured human-readable yaml schema](./autodocodec-yaml)
* [A Swagger schema](https://swagger.io/specification/v2/)
* [An Openapi schema](https://swagger.io/specification/)

See [the golden test directory](./autodocodec-api-usage/test_resources) directory for example outputs.

## Features

* ✓ Correct-by-construction encoding and decoding, without generating code.
* ✓ Generate automatically-correct documentation from code.
* ✓ Support for recursive types.

## State of this project

This project is ready to try out!


## Fully featured example

``` haskell
data Example = Example
  { exampleTextField :: !Text,
    exampleIntField :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving
    ( FromJSON, -- <- FromJSON instance for free.
      ToJSON, -- <- ToJSON instance for free.
      Swagger.ToSchema, -- <- Swagger schema for free.
      OpenAPI.ToSchema -- <- OpenAPI schema for free.
    )
    via (Autodocodec Example)

instance HasCodec Example where
  codec =
    object "Example" $
      Example
        <$> requiredField "text" "documentation for the text field" .= exampleTextField
        <*> requiredField "int" "documentation for the int field" .= exampleIntField
```

## Tests

While we don't provide any actual guarantees, we do have tests for the following properties that we would like to maintain:

* [Encoding and decoding roundtrips through JSON.](./autodocodec-api-usage/test/Autodocodec/AesonSpec.hs)
* [For standard types, encoding behaves in the same way that `aeson` does.](./autodocodec-api-usage/test/Autodocodec/AesonSpec.hs)
* [Error messages for decoding are still good.](./autodocodec-api-usage/test/Autodocodec/AesonSpec.hs)
* [Generated Human-readible documentation looks good.](./autodocodec-api-usage/test/Autodocodec/Yaml/DocumentSpec.hs)
* [Generated JSON schemas look good.](./autodocodec-api-usage/test/Autodocodec/Aeson/SchemaSpec.hs)
* [Generated Swagger schemas look good.](./autodocodec-api-usage/test/Autodocodec/SwaggerSpec.hs)
* [Generated OpenAPI schemas look good.](./autodocodec-api-usage/test/Autodocodec/OpenAPISpec.hs)
* [Generated values are accepted by the corresponding generated JSON schemas.](./autodocodec-api-usage/test/Autodocodec/Aeson/SchemaSpec.hs)
* [Generated values are accepted by the corresponding generated Swagger schemas.](./autodocodec-api-usage/test/Autodocodec/SwaggerSpec.hs)
* [Generated values are accepted by the corresponding generated OpenAPI schemas.](./autodocodec-api-usage/test/Autodocodec/OpenAPISpec.hs)
* [Encoding and decoding roundtrips through YAML.](./autodocodec-api-usage/test/Autodocodec/YamlSpec.hs)
* [We try to make sure that backward compatibility is maintained.](./autodocodec-api-usage/src/Autodocodec/Usage.hs)
* [Codecs are more or less inspectable.](./autodocodec-api-usage/test/Autodocodec/ShowSpec.hs)
* [TODO: Encoding and decoding is still fast](TODO)
