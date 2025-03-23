1- marked-v1.sc is the implementation of the marked approach in Scala, using only Booleans.

2- marked-Weightedv1.sc is the implementation of the semiring approach, currently tested only with the Boolean semiring.

3- marked-Weightedv2.sc is the implementation of the Leftmost semiring, which finds the leftmost matching, or returns NoLeft if none is found.

4- marked-Weightedv3.sc is the implementation of the Leftlong semiring, which finds the left-longest matching, or returns NoLeftLong if none is found.

5- marked-SemiringLeftMost.sc is the standalone implementation of the Leftmost semiring. It is based on the structure of play_init, and uses the nullable and fin functions.
The file includes helper functions (helperf and helperft) that help construct CHAR with the appropriate semantics. These helpers are used like CHAR(helperft('a')) or CHAR(helperf('a')) to create a function for CHAR that accepts a tuple or char and returns either semiring.index(pos), semiring.one, or semiring.zero.

6- marked-SemiringLeftLong.sc is the standalone implementation of the Leftlong semiring. It follows the same structure and logic as marked-SemiringLeftMost.sc, including the use of nullable, fin, and the same helper functions for CHAR construction.

7- marked-intNTIMES.sc is the implementation of the marked approach, but using a list of integers instead of Boolean marks.

8- marked-BitcodeTest.sc is the first implementation of bitcodes for the marked approach. It uses a list of bits attached to each regular expression constructor (enum Rexpb).

9- play_point.sc is the implementation of the marked approach using a Point constructor to replace Boolean marks. The POINT constructor is introduced as a wrapper around CHAR to indicate that it is marked, instead of adding a Boolean flag.

10- play_point2.sc extends the Point constructor implementation. The POINT constructor now holds a list of integers as a tag, instead of being a simple wrapper. This list records the history of marked CHARs:
0 indicates that the CHAR was once marked, and 1 indicates it is currently marked.
A function popPoints is introduced here, which receives the final marked regular expression and "pops" the regular expression at each stage of the input string. It returns a list of marked regular expressions of size equal to the length of the input string.

11- play_point3.sc combines play_point.sc (Point constructor) and play_point2.sc (tag list), and adds bitcode implementation by attaching a list of bits to each regular expression constructor.

12- play_Point3_noTags.sc is the same as play_point3.sc, but without the tags—only the bitcodes implementation remains.

13- play_point3_BIT.sc is an attempt to implement a new constructor BIT, which is the only constructor that holds bit information. It is responsible for collecting bits, instead of having bit lists in all constructors.