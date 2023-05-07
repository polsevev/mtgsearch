## How to run the program

In order to run this program one has to install Stack

One can download Stack [here](https://docs.haskellstack.org/en/stable/)

Then to verify it works run:
```
stack run 
```
Before the program can be tested, we have to fill the database with data. There does exist a version to seed the data directly from the Scryfall API, however this takes about 20 minutes (80K object long JSON file that has to be parsed)

Therefore i have provided a smaller JSON file containing less cards. In order to seed the database with this you can use
```
stack run -- --seedFromFile
```

If you want to wait the full 20 minutes you can run the command
```
stack run -- --seedFromWeb
```

## DSL

Will be a language based in set theory, where queries can be combined, negated etc etc. 


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
3. (`CmcLTEQ` Int)
    - This function will find all cards with converted mana cost less than or equal to the Integer
3. (`CmcMTEQ` Int)
    - This function will find all cards with converted mana cost less than or equal to the Integer
4. (`SuperType` String)
    - This function will find all cards with the String value in its type_field. 
    - NOTE: It is recommended to only use one word at a time and rather use set operators for better matching
5. (`NotSuperType` String)
    - This function will find all cards not containing the String value in its typefield
    - NOTE: It is recommended to only use one word at a time and rather use set operators for better matching
5. (`Color` String)
    - This function allows to find all cards containing specific color/colors.
    - VALID INPUT: [W,R,G,B,U] where U = Blue and B = Black
    - EX: a valid query is (Color WRG) fill find all cards that are White, Red and Black

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

## Example Queries

Here are some example queries and their expected results:

`((SuperType Creature) intersect (SuperType Legendary)) intersect (CmcLTEQ 3)` this query should return all Creatures that are also legendary with a converted mana cost less than or equal to three

`(SuperType Enchantment) intersect (SuperType Creature)` This should return all Enchantments that are also Creatures

`(SuperType Creature) minus (SuperType Legendary)` This should return all non legendary creatures. 

## Lexer

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