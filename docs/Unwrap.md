# The "Unwrap" Process

## Introduction
Always, we as human can easily prove a proposition by using some "high-level" or "indirect" proving techniques, like the deduction theorem（演绎定理） or negation front theorem (否定前件律).

But, the fact is that, we need its most primitive form sometimes.

For theorems like "negation front theorem", since its prove involves no participation of "deduction theorem", so intuitively, we can do a simply substitution into the hardwired proving pattern.


But for the ones using "deduction theorem", things are a little harder to track, since we actually used a special proving technique in the textbook.

However, I found a way of "unwrapping" the deduction theorem formally and mechanically.

## The Unwrapping Technique for the Deduction Theorem

Assume that we want to prove $$\vdash (p \to q)$$, we first claim in our "indirect proof" that, "we can equivalently prove $$\{p\} \vdash q$$"

Then, we will track all usage of assumption $$p$$ in our indirect proof, as follows:

### A Formal Analysis

Step|Proposition|Reason
--- | --------- | ----
(1) | ... |...
... | ... |...
($$a_1$$) | $$p \to A$$ | $$T_1$$
($$a_1 + 1$$) | $$p$$ | assumption
($$a_1 + 2$$) | $$A$$ | MP with $$a_1$$, $$a_1 + 1$$
... | ... | ...
($$a_2$$) | $$A \to B$$ | $$T_2$$
($$a_2 + 1$$) | $$B$$ | MP with $$a_1 + 2$$, $$a_2$$
...|...|...
($$a_3$$) | $$B \to q$$ | $$T_3$$
($$a_3 + 1$$) | $$q$$ | MP with $$a_2 + 1$$, $$a_3$$
Qed|

Then, we can do the unwrapping like this to prove $$\vdash (p \to q)$$ directly:

Step|Proposition|Reason
--- | --------- | ----
(1) | ... |...
... | ... |...
($$a_1$$) | $$p \to A$$ | $$T_1$$
... | ... | ...
($$a_2 - 1$$) | $$p \to (A \to B)$$ | $$T_2$$ with a harmless prefix
($$a_2$$) | $$(p \to A) \to (p \to B)$$ | L2 and MP with $$a_2 - 1$$
($$a_2 + 1$$) | $$p \to B $$ | MP with $$a_1$$, $$a_2$$
...|...|...
($$a_3 - 1$$) | $$ p \to (B \to q) $$ | $$T_3$$ with a harmless prefix
($$a_3$$) | $$(p \to B) \to (p \to q)$$ | L2 and MP with $$a_3 - 1$$
($$a_3 + 1$$) | $$p \to q$$ | MP with $$a_2 + 1$$, $$a_3$$
Qed|

### Some shorthands

I used "with a harmless prefix" base on the fact that

Step|Proposition|Reason
--- | --------- | ----
1   | $$p$$ | $$T$$
2   | $$p \to (w \to p)$$ | Positive Front Theorem
3   | $$(w \to p)$$ | MP with 1, 2

Here, we add a "harmless" $$w$$ to the front of $$p$$

I also used the "L2 and MP", which could be formalized as below:

Step|Proposition|Reason
--- | --------- | ----
1   | $$ p \to (q \to w)$$ | $$T$$
2   | $$ (p \to (q \to w)) \to ((p \to q) \to (p \to w)) $$ | L2
3   | $$(p \to q) \to (p \to w) $$ | MP with 1 and 2

Which is just like a "distribution law".

### The ideas behind the unwrapping process

And I need to give you a more abstract view of this method. From the formal example above, we can see that there seem to be **some kind of chain**, which links the propositions together. All usual

Step|Proposition|Reason
--- | --------- | ----
($$a_2$$) | $$A \to B$$ | $$T_2$$
($$a_2 + 1$$) | $$B$$ | MP with $$a_1 + 2$$, $$a_2$$


will become 

Step|Proposition|Reason
--- | --------- | ----
($$a_2 - 1$$) | $$p \to (A \to B)$$ | $$T_2$$ with a harmless prefix
($$a_2$$) | $$(p \to A) \to (p \to B)$$ | L2 and MP with $$a_2 - 1$$
($$a_2 + 1$$) | $$p \to B $$ | MP with $$a_1$$, $$a_2$$


This actually make me think about the "Monad" in Haskell, in which there are things like:

	liftM :: Monad m => (a1 -> r) -> m a1 -> m r
	(>>=) :: Monad m => m a -> (a -> m b) -> m b


Using these methods, we can **selectively wrap and unwrap the proving**, and the best thing is that, it can be easily done by machine.

## A Hands-on Practise: 肯定否定律

To give you a more real feeling, we will see the prove of 肯定否定律 ($$\vdash (\neg p \to p) \to p$$):


The human proving: We will only need to prove $$\{ \neg p \to p \} \vdash p$$ by deduction theorem.

Step|Proposition|Reason
--- | --------- | ----
1|$$\neg p \to (p \to \neg (\neg p \to p))$$| Negative Front Theorem
2|$$(\neg p \to (p \to \neg (\neg p \to p))) \to ((\neg p \to p) \to (\neg p \to \neg (\neg p \to p))))$$ | L2
3|$$(\neg p \to p) \to (\neg p \to \neg (\neg p \to p)))$$| MP with 1 2
4|$$\neg p \to p $$ | Assumption
5|$$\neg p \to \neg (\neg p \to p)) $$| MP with 3 4
6|$$(\neg p \to \neg (\neg p \to p))) \to ((\neg p \to p) \to p) $$ | L3
7|$$(\neg p \to p) \to p$$ | MP with 5 6
8|$$p$$ | MP with 4 7

We will first embed the proof of "Negative Front Theorem" into it:

Step|Proposition|Reason
--- | --------- | ----
-6|$$(\neg (\neg (\neg p \to p)) \to \neg p) \to (p \to (\neg (\neg p \to p)))$$|L3
-5|$$((\neg (\neg (\neg p \to p)) \to \neg p) \to (p \to (\neg (\neg p \to p))) \to (\neg p \to ((\neg (\neg (\neg p \to p)) \to \neg p) \to (p \to (\neg (\neg p \to p))))))$$| L1
-4|$$\neg p \to ((\neg (\neg (\neg p \to p)) \to \neg p) \to (p \to (\neg (\neg p \to p))))$$| MP with -6, -5
-3|$$(\neg p \to ((\neg (\neg (\neg p \to p)) \to \neg p) \to (p \to (\neg (\neg p \to p))))) \to ((\neg p \to (\neg (\neg (\neg p \to p)) \to \neg p)) \to (\neg p \to (p \to (\neg (\neg p \to p)))))$$| L2
-2|$$(\neg p \to (\neg (\neg (\neg p \to p)) \to \neg p)) \to (\neg p \to (p \to (\neg (\neg p \to p))))$$| MP with -4, -3
-1|$$\neg p \to (\neg (\neg (\neg p \to p)) \to \neg p)$$| L1
0|$$\neg p \to (p \to (\neg (\neg p \to p)))$$| MP with -2, -1
1|$$\neg p \to (p \to \neg (\neg p \to p))$$| Negative Front Theorem
2|$$(\neg p \to (p \to \neg (\neg p \to p))) \to ((\neg p \to p) \to (\neg p \to \neg (\neg p \to p))))$$ | L2
3|$$(\neg p \to p) \to (\neg p \to \neg (\neg p \to p)))$$| MP with 1 2
4|$$\neg p \to p $$ | Assumption
5|$$\neg p \to \neg (\neg p \to p)) $$| MP with 3 4
6|$$(\neg p \to \neg (\neg p \to p))) \to ((\neg p \to p) \to p) $$ | L3
7|$$(\neg p \to p) \to p$$ | MP with 5 6
8|$$p$$ | MP with 4 7


Here, we can see that only step 4 used "assumption", so we can trace up and get:

Step|Proposition|Reason
--- | --------- | ----
1 |$$ (\neg (\neg (\neg p \to p)) \to \neg p) \to (p \to (\neg (\neg p \to p))) $$| L3
2 |$$ ((\neg (\neg (\neg p \to p)) \to \neg p) \to (p \to (\neg (\neg p \to p)))) \to (\neg p \to ((\neg (\neg (\neg p \to p)) \to \neg p) \to (p \to (\neg (\neg p \to p))))) $$|  L1
3 |$$ \neg p \to ((\neg (\neg (\neg p \to p)) \to \neg p) \to (p \to (\neg (\neg p \to p)))) $$|  MP with 1, 2
4 |$$ (\neg p \to ((\neg (\neg (\neg p \to p)) \to \neg p) \to (p \to (\neg (\neg p \to p))))) \to ((\neg p \to (\neg (\neg (\neg p \to p)) \to \neg p)) \to (\neg p \to (p \to (\neg (\neg p \to p))))) $$|  L2
5 |$$ (\neg p \to (\neg (\neg (\neg p \to p)) \to \neg p)) \to (\neg p \to (p \to (\neg (\neg p \to p)))) $$|  MP with 3 4
6 |$$ \neg p \to (\neg (\neg (\neg p \to p)) \to \neg p) $$|  L1
7 |$$ \neg p \to (p \to (\neg (\neg p \to p))) $$|  MP with 5, 6
8 |$$ (\neg p \to (p \to (\neg (\neg p \to p)))) \to ((\neg p \to p) \to (\neg p \to (\neg (\neg p \to p)))) $$| L2
9 |$$ (\neg p \to p) \to (\neg p \to \neg (\neg p \to p)) $$| MP 7 8
Let|$$ k = (\neg p \to \neg (\neg p \to p)) $$|
Let|$$ q = (\neg p \to p) $$|
10 |$$ k \to (q \to p) $$| L3
11 |$$ (k \to (q \to p)) \to ((k \to q) \to (k \to p)) $$| L2
12 |$$ (k \to q) \to (k \to p) $$| MP 10, 11
13 |$$ ((k \to q) \to (k \to p)) \to (q \to ((k \to q) \to (k \to p))) $$| L1
14 |$$ q \to ((k \to q) \to (k \to p)) $$| MP 12, 13
15 |$$ (q \to ((k \to q) \to (k \to p))) \to ((q \to (k \to q)) \to (q \to (k \to p))) $$| L2
16 |$$ (q \to (k \to q)) \to (q \to (k \to p)) $$| MP 14, 15
17 |$$ q \to (k \to q) $$| L1
18 |$$  q \to (k \to p) $$| MP 16, 17
19 |$$ (q \to (k \to p)) \to ((q \to k) \to (q \to p)) $$| L2
20 |$$ (q \to k) \to (q \to p) $$| MP 12 13
21 |$$ q \to p $$| MP 9 14

During the Middle of proving, I use $$k$$ and $$q$$ to represent a formula, but this doesn't harm the primitivity of our induction, just for the convenience of reading.


We can see that, although the principle of "unwrapping" is not the optimal solution, but one that could be executed by computer, as long as human gives a basic guide of directions.

## More

Actually, I am on the way of building the "unwrapping" on the machine as a program, which could output the primitive proof after receiving human's guidance.

I have finished the basic part of [a simple proof assistant](https://github.com/izgzhen/logix) supporting propositional induction in Haskell. Now I can prove the identity theorem, or $$\vdash p -> p$$ on it. But it is **very primitive** now.

In the next month, I will try to add the features of stoting the proof of a specific theorem, as will as the use of deduction theorem. So I can implement the unwrapping algorithm on this proof assistant.