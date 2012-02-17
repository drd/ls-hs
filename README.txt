...........................
.                         .
.    CS457 Winter 2012    .
.    Project Proposal     .
.    Eric O'Connell       .
.                         .
...........................


3-Dimensional Lindenmayer Systems in Haskell
============================================


Overview
--------

Lindenmayer Systems are recursive, self-rewriting grammars which can approximate
growth patterns observed in nature, most commonly known for the branching
structures in plants and trees. The grammars generate strings which can be
interpreted as part of a sophisticated Logo/Turtle-graphics command, generating
2 and 3-dimensional images in the process.


Example
-------

A Lindenmayer grammar (L-Grammar) is a 3-tuple composed of the alphabet, the
start or initial string, and a list of productions. To produce the 2nd iteration
of an L-grammar, one walks the start string, replacing each existing character
with a matching production rule, or itself if no productions match, into a new
string. To illustrate:

Grammar:
| alphabet = { F + - }
| start = F + F + F
| productions:
|   F -> F + F - F + F

Iterations (parenthesis added for some clarity):
1 : F+F+F
2 : (F+F-F+F)+(F+F-F+F)+(F+F-F+F)
3 : ( (F+F-F+F) + (F+F-F+F) - (F+F-F+F) + (F+F-F+F) ) + ( (F+F-F+F) + (F+F-F+F)
    - (F+F-F+F)) + (F+F-F+F) ) + ( (F+F-F+F) + (F+F-F+F) - (F+F-F+F) + (F+F-F+F) )
4 : ...

To interpret these strings, we need some additional information:

Commands:
| F : from the current point, draw a line of d pixels
| + : rotate the turtle theta degrees clockwise
| - : rotate the turtle theta degrees anticlockwise

Parameters:
| theta : 60deg
| d     : 50px


Timeline
--------

Week  6 : Initial exploration of data structures + OpenGL in Haskell
Week  7 : Begin to develop context-free L-grammars + draw 2d primitives
Week  8 : Finish context-free L-grammars, draw 2d branching patterns
Week  9 : Add 3d state to turtle, 3d commands, draw 3d branching patterns
Week 10 : Add support for polygons, colors, context-sensitive grammars


Current Status
--------------

I read "The Algorithmic Beauty of Plants" when I was 12, and I've re-read
it every few years since. It's been a long time since I've been able to justify
playing with these ideas, though, and Haskell seems like a great place to do so.

Since this project involves some potentially annoying* integration aspects,
I have downloaded and begun to experiment with various OpenGL bindings for
ghc, and have successfully run some example programs from the nehe-tuts
package. Since I've actually worked with the NeHe examples before in C, I have
a decent starting point on which to map my understanding into Haskell.


Sources
-------

1. http://en.wikipedia.org/wiki/L-system was helpful in reminding the specific
details of the grammar tuple.
