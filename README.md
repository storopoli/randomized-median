# Randomized Median Algorithm

Some computations that are intractable deterministically can be replaced with randomized algorithms,
if you are willing to replace the deterministic computed quantity with a probabilistic approximated interval.

This is a simple implementation in Haskell of the "Randomized Median Algorithm" described
in the textbook "Probability and Computing" by Michael Mitzenmacher and Eli Upfal.

If you want to learn more about the algorithm, check my blog post [here](https://storopoli.com/blog/randomness).

## Usage

```bash
stack run
```

## Performance

```bash
============================
Testing with 10_000_001 shuffled elements

Exact median calculation:
  Result: 5000001.0
  Time: 18.906611 seconds

Randomized approximate median calculation:
  Result: 5000001.0
  Time: 1.095511 seconds

Error percentage: 0.0000%
Speedup factor: 17.26x
```
