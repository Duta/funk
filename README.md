Funk
====

Initially this will just be the Core language,
from `Implementing Functional Languages`,
however with time it will become sufficiently
different to warrant its own name.

Differences from Core:

    ~=    is !=
    &     is &&
    |     is ||
    I     is id
    \x. y is \x -> y

Precedence:

    Precedence | Associativity | Operator
    ----------------------------------------
    6          | Left          | Application
    5          | Right         | *
    5          | None          | /
    4          | Right         | +
    4          | None          | -
    3          | None          | ==
    3          | None          | !=
    3          | None          | <
    3          | None          | >
    3          | None          | <=
    3          | None          | >=
    2          | Right         | &&
    1          | Right         | ||
