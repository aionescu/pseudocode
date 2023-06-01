function factorial(n: Int): Int
  if n < 2 then
    return n
  else
    return n * factorial(n - 1)
  end
end

program
  write factorial(read "n: ")
end
