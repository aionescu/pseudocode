function factorial(n: Int): Int
  write "factorial of ", n
  if n < 2 then
    return n
  else
    return n * factorial(n - 1)
  end
end

program
  write "Running"
  let x = factorial(5)
end
