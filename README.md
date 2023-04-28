# mtgsearch


Developement of a custom magic card search engine. 

# DSL

Will be a language based in set theory, where queries can be combined, negated etc etc. 
Example.

# Lexer

This languages lexer is very dumb. Therefore you the user has to be very verbose with your parenthesis. Example:

This will not work:
```
(IS Instant) union (Color Red) union (Color Green)
```
However this will
```
((IS Instant) union (Color Red) union (Color Green))
```