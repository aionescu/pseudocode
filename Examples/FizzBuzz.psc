function fizzBuzz(n: Int)
  if n % 15 == 0 then
    write "FizzBuzz"
  else if n % 3 == 0 then
    write "Fizz"
  else if n % 5 == 0 then
    write "Buzz"
  else
    write n
  end
end

program
  for i = 1 to read "n: " do
    fizzBuzz(i)
  end
end
