# Task 1
- Add a test to check that in no path long at most 100 states mutual  exclusion fails (no more than 1 writer, and no readers and writers together)

Simply explain how you structured the tests (property check) and in particular the one that asked you to add that
is `Mutual exclusion for path`.

- Can you extract a small API for representing safety properties?

Yes, It's in `SystemPropertyAnalisis`, a general property could be represented as `SystemProperty[S]` that is a function
from a state to a boolean. Then you add `invariant`, `eventually` and `never` extension to System ADT, then show how to
use it (refeer to the test done before)

- What other properties can be extracted?

In general you can test any property that is a function from a state to a boolean. So other than safety properties you 
can add liveness, fairness, etc

- How the boundness assumption can help

The fact that we're working with bounded paths allows us to reason about the system in a finite (extensional) way, making it easier to 
prove properties like mutual exclusion, absence of deadlocks, and other safety properties.

