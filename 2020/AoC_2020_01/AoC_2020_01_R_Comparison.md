# Overview

These two solutions solve the same problem in the same basic way: find values that complement each other to a target total of 2020. The original solution uses a set-membership check against a delta vector for part 1 and an `expand.grid` cross-product for part 2. The Copilot solution uses a direct lookup approach with a hash-like environment for part 1 and a nested-pair scan with complement checks for part 2. The algorithms are mathematically equivalent, but the Copilot version is more explicit and avoids constructing a full Cartesian product in part 2.

## My Original R Solution

The original solution is a compact R-centric approach built around the observation that if `x` is in the expense list then a valid pair must satisfy `2020 - x` being present as well.

For part 1:

- Convert the raw input to numeric values.
- Compute `Delta <- 2020 - X`.
- Check which `Delta` values are also in `X` using `which(Delta %in% X)`.
- Multiply the matching values together with `prod(X[Which])`.

For part 2:

- Compute `Delta <- 2020 - X`.
- Create every pair combination with `expand.grid("A" = X, "B" = X)`.
- Sum rows with `rowSums` and test whether the pair sum is in `Delta`.
- Use `unique(Pairs$A[Which])` and multiply the remaining values to get the product.

This is a clever but brute-force-heavy approach for part 2 because it materializes a full `n x n` grid before filtering.

## Copilot R Solution

The Copilot solution keeps the same core idea but makes the lookup explicit.

For part 1:

- Read the expense list as integers.
- Store each value in a hash-like environment keyed by its string representation.
- For each value, check whether `2020 - value` was already seen.
- Return `value * (2020 - value)` immediately when a match is found.

For part 2:

- Build a lookup set of all expense values.
- Iterate over all pairs `(i, j)` with `j > i`.
- Compute `complement <- 2020 - expenses[i] - expenses[j]`.
- If `complement` is present in the lookup set and is distinct from the chosen values, return the triple product.

This keeps the same logic as the original, but the search is structurally simpler and more scalable than building a full cross-product table.

## Key Conceptual and Methodological Differences

The main conceptual difference is not in the overall approach, but in how the matching condition is searched:

- Original: use vector membership for pairs and a full Cartesian expansion for triples.
- Copilot: use a direct lookup set and nested pairing logic to avoid materialising all combinations up front.

In other words, both solutions rely on the same complement rule `a + b + c = 2020`, but the Copilot version makes the complement search more direct and less dependent on large intermediate objects.

## Efficiency and Scalability

The original part 1 solution is effectively O(n) for membership testing once `X` is converted to a vector, which is efficient. The part 2 approach is much heavier because `expand.grid` generates up to `n^2` rows before filtering, so its memory cost is quadratic and the overhead is avoidable.

The Copilot solution keeps part 1 at O(n) and part 2 at O(n^2) in the pair scan, but it avoids the extra `n^2` intermediate table and is more memory-conscious. In practice, this is a meaningful improvement when the input is larger or when a file is processed repeatedly in a tight loop.

## Lessons and Takeaways

- The complement trick is the real insight: the key operation is to ask which value is needed to reach 2020.
- Hash-based membership checks are a natural fit in R when performance matters, especially for repeated lookups.
- For part 2, a full Cartesian product is easy to write but not ideal; a pair loop plus complement lookup is usually a cleaner and more scalable pattern.
- The original solution is elegant and concise, but the Copilot solution makes the search strategy clearer and more explicit about avoiding unnecessary data structures.
