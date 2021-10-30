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


