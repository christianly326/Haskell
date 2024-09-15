-- Function that takes an integer and return the fatorial of that number
-- Christian Michael Lagura Yacapin

factorial :: Int -> Int

factorial 0 = 1
factorial n = n * factorial(n - 1)
