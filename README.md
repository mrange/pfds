pfds - Purely Functional Data Structures
========================================

One of the best programming books I've read is [Purely Functional Data Structures](http://www.amazon.com/Purely-Functional-Structures-Chris-Okasaki/dp/0521663504)
by Chris Okasaki.

pfds is a set of data structures from the book implemented in F#. My intention is to grow pfds to include all relevant datastructures from the book together with guidelines when a specific data structure is relevant.

One thing I learned from the book is that each data structure have their strengths and weaknesses.

For example; no data structures in pfds can beat a simple queue implemented as an array with start and stop indices in terms of performance and memory footprint. However, this simple queue isn't immutable (functional).

If immutability is important adding immutability to the simple queue trivially by forcing a full copy whenever pop/pushing to the queue can prove too expensive.

pfds provides several immutable implementations of queues each with their own set of pros/cons:
1. BatchedQueue - A simple immutable Queue where cost of head op is O(1) and tail/snoc is O(1) amortized. The cost is that tail/snoc is O(n) worst-case
2. StreamedQueue - Immutable Queue where the cost tail/snoc is mitigated by using lazy evaluation. The cost is a more complex data structure with lower defacto performance for head and increased memory footprint.
3. BootstrappedQueue - Immmutable Queue where head/tail/snoc all run in O(1) (effectively). The cost is more complex code and dats structure.


