### Binary search trees

Right now there is only an implementation of AVLTrees. With AVLTrees we optimize for lookups, since the tree's balance is mantained on each insertion then the lookups for each of the min and max range values would be O(log(n)). We then prune the tree of the extraneous nodes and traverse to get back the desired range of operations, which would make lookup on value ranges {min,max} a linear O(n) operation in the worst case.

Balance is mantained by rotating the trees in different ways depending on the balance after insertion, always mantaining the invariant of the BST. There is a suite of tests for different cases to test rotations and balanced being preserved.
