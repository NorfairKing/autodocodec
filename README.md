# Autodocodec

Autodocodec is short for "self(auto)- documenting encoder and decoder".

In short:
You write one (1) instance, of the 'Codec' type-class, for your type, and you get:

* [A 'ToJSON' instance from 'aeson'](https://hackage.haskell.org/package/aeson-2.0.1.0/docs/Data-Aeson-Types.html#t:ToJSON)
* [A 'FromJSON' instance from 'aeson'](https://hackage.haskell.org/package/aeson-2.0.1.0/docs/Data-Aeson-Types.html#t:FromJSON)
* [A json schema](http://json-schema.org/)
* A human-readable yaml schema
* Hopefully soon also: A swagger schema
* Hopefully soon also: An openapi schema
* Potentially also: bson instances for mongodb

## DISCLAIMER: This is a work in progress.

This is not ready for production, it is not in use in any of my own projects yet, I do not recommend using it yet.

* Documentation is not complete.
* Some pieces of the implementation

## Goals:

* Correct-by-construction encoding and decoding, without generating code.
* Generate automatically-correct documentation from code.
* Fun but not important: Be able to provide instances without depending on aeson and/or yaml.
* Would be nice: support for recursive types.


## Fully featured example

TODO

## Tests

While we don't provide any actual guarantees, we do have tests for the following properties that we would like to maintain:

* [Encoding and decoding roundtrips.](./autodocodec-api-usage/test/Autodocodec/AesonSpec.hs)
* [For standard types, encoding behaves in the same way that `aeson` does.](./autodocodec-api-usage/test/Autodocodec/AesonSpec.hs)
* [Error messages for decoding are still good.](./autodocodec-api-usage/test/Autodocodec/AesonSpec.hs)
* [Generated Human-readible documentation looks good.](./autodocodec-api-usage/test/Autodocodec/Yaml/DocumentSpec.hs)
* [Generated JSON schemas look good.](./autodocodec-api-usage/test/Autodocodec/Aeson/SchemaSpec.hs)
* [Generated Swagger schemas look good.](./autodocodec-api-usage/test/Autodocodec/SwaggerSpec.hs)
* [Generated OpenAPI schemas look good.](./autodocodec-api-usage/test/Autodocodec/OpenAPISpec.hs)
* [Generated values are accepted by the corresponding generated JSON schemas.](./autodocodec-api-usage/test/Autodocodec/Aeson/SchemaSpec.hs)
* [Generated values are accepted by the corresponding generated Swagger schemas.](./autodocodec-api-usage/test/Autodocodec/SwaggerSpec.hs)
* [Generated values are accepted by the corresponding generated OpenAPI schemas.](./autodocodec-api-usage/test/Autodocodec/OpenAPISpec.hs)
* [We try to make sure that backward compatibility is maintained.](./autodocodec-api-usage/src/Autodocodec/Usage.hs)
* [Codecs are more or less inspectable.](./autodocodec-api-usage/test/Autodocodec/ShowSpec.hs)
* [TODO: Encoding and decoding is still fast](TODO)
