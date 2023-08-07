let fastfib_aux h pp p n =
    if n <= 1 then 1 else h (n - 1) p (pp + p);;
