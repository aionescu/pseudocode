function fibonacci(n: Int): Int
  if n < 2 then -- a
    return 1
  else
    return fibonacci(n - 1) + fibonacci(n - 2)
  end
end

program
  for i = 0 to 10 do
    write fibonacci(i)
  end
end
