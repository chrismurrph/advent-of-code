
Uses Instaparse for parsing, unless needs to be dynamic, in which case simple reduction used (for instance day 9).

Day 8 is done with simple hardware instructions, with no need for any matrix operations, which means essentially the
same code can be used whether rotating a column or a row.

How could have done better
--------------------------

Day 1
1. `read-string` can put into vector and ignore commas
2. As part of this need to `map name` so symbols become strings
3. As first of each always a letter easy to vector into char and int
4. Each direction can be a number from 0->3 incl if use reductions
5. Easy to map these into a stream of one of [0 1] [1 0] [0 -1] [-1 0]
6. repeat to multiply do each one
7. another reductions to get all the positions
8. first command where crossed over all previous commands (for second part)
