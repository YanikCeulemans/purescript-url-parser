# purescript-url-parser
An rfc1738 compatible url parser written in PureScript

This project's primary purpose is for me to explore monadic parser combinators.

The URL spec used in this project is from [RFC 1738, section 3.3](https://datatracker.ietf.org/doc/html/rfc1738#section-3.3). For now, the only implemented URLs are http and https. It should be trivial to add others.

The `Url` module provides the parsers `httpURL` and `httpsURL` from the `Text.Parsing.StringParser` module. For now, they can be run with the following snippet:
```purescript
runParser httpURL "https://github.com"
```
This will give you either a `ParseError` or a `Url`. A `Url` can be stringified using the `toString` function.
See the unit tests for more information.

Future improvements:
- [x] Provide a convenience function to parse urls. This will prevent the need to depend on `Text.Parsing.StringParser` as a consumer of this module.
- [ ] Provide more functions to manipulate urls.
- [ ] Prevent invalid `Url` construction by using smart constructors. It is now possible to create a negative port number for example.
