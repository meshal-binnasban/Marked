marked-v1.sc is the implementation of the marked approach in scala, only Boolean.
marked-Weightedv1.sc is the implementation of the semiring , currently only tested the boolean semiring.
marked-Weightedv2.sc is the implementation of the semiring Leftmost which finds the left most matching and if not, NoLeft.
marked-Weightedv3.sc is the implementation of the semiring Leftlong which finds the left longest  matching and if not, NoLeftLong.

marked-SemiringLeftMost.sc is the implementation of the semiring Leftmost by itself. It is based on the structure of play_init. it uses the nullable and fin functions. The file includes helper functions (helperf and helperft) that help construct CHAR  with the appropriate functions. These helpers are used like CHAR(helperft('a')/ helperf('a') )  to create a function for CHAR that accepts a tuple/char and returns either semiring.index(pos), semiring.one, or semiring.zero.

marked-SemiringLeftLong.sc is the implementation of the semiring Leftlong by itself. It is based on the structure of play_init. it uses the nullable and fin functions. The file includes helper functions (helperf and helperft) that help construct CHAR  with the appropriate functions. These helpers are used like CHAR(helperft('a')/ helperf('a') )  to create a function for CHAR that accepts a tuple/char and returns either semiring.index(pos), semiring.one, or semiring.zero.