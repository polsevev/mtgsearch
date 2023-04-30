# mtgsearch


Developement of a custom magic card search engine. 

# DSL

Will be a language based in set theory, where queries can be combined, negated etc etc. 
Example.

I had to simplify the DSL a lot in order to have time. I have never written a lexer or parser before. So doing that in a language i am not that familiar with (haskell) proved to be quite difficult. Therefore in this section i will relay exactly how this DSL works. 

It essentially is a Query Language, with similar properties as SQL. (It uses SQL at the `bottom` layer). So we have split the two `Sets` we can to into the following. 

## Query
A query is a very simple way of saying, limit all the cards in magic the gathering with the set limitation. Example there exists a Query function `SuperType` that takes in a string. This function will then match all cards `type_line` and return only those containing a match.

## Operator
This is an operator between two sets. Example of an operator is the `intersect` operator. This takes two `Sets` and returns only the cards that exist in both. Example of a query using `intersect`:

```
(SuperType Creature) intersect (SuperType Legendary)
```
This query will only return Legendary Creatures, (Note: In theory you can write only (SuperType Legendary Creature) and it will return the same, however it is recommended to limit the SuperType search to only one word)

## Avalible Query functions:

1. (`CmcLT` Int)
    - This function will find all cards with converted mana cost less than the Integer provided as input
2. (`CmcMT` Int)
    - This function will find all cards with converted mana cost more than the Integer provided as input
3. (`CmcEQ` Int)
    - This function will find all cards with converted mana cost equal to the Integer
4. (`SuperType` String)
    - This function will find all cards with the String value in its type_field. 
    - NOTE: It is recommended to only use one word at a time and rather use set operators for better matching
5. (`Color` Char)
    - This function allows to find all cards of specific color.
    - VALID INPUT: [W,R,G,B,U] where U = Blue and B = Black

## Avalible Operators:
1. left `intersect` right
    - This operator will give the intersect of whatever is to the left and right of it.
    - Example: (SuperType Creature) intersect (Color R) = All Red Creatures
2. left `union` right
    - This operator will give the union of left and right
    - Example: (SuperType Creature) union (SuperType Instant) = All Creatures and Instants
3. left `minus` right
    - This operator will give you all cards present in left but not in right
    - Example: (SuperType Creature) union (SuperType Legendary) = All non Legendary Creatures
# Lexer

This languages lexer is very dumb. Therefore you the user has to be very verbose with your parenthesis. Example:

This will not work:
```
(IS Instant) union (Color Red) union (Color Green)
```
However this will
```
((IS Instant) union (Color Red)) union (Color Green)
```

The outer parenthesis is not needed, ex `SuperType Creature` is equivalent to `(SuperType Creature)`
