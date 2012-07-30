-- Tail-recursive functions will use
-- constant stack space
sum 0 acc = acc
sum n acc = sum (n - 1) (acc + n)
