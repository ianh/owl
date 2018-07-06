#!using bluebird.v1

# This is "json-ish" because bluebird's built-in tokenizer doesn't interpret
# string escape sequences the way JSON does.

value =
 [ '{' (string ':' value (',' string ':' value)*)? '}' ] : object
 [ '[' (value (',' value)*)? ']' ] : array
 string : string
 json-number : number
 'true' : true
 'false' : false
 'null' : null

json-number =
 '-' number : negative
  number : positive
