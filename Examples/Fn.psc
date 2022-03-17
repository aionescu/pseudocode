function f(x: Int): Int
  if x == 5 then
    return x
  else
    return f(x + 1)
  end
end

program
  let x = f(2)
  write x
end
