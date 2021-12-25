let n: Int = read "Enter n: "

for i = 1 to n do
  if i % 15 == 0 then
    write "FizzBuzz"
  else if i % 3 == 0 then
    write "Fizz"
  else if i % 5 == 0 then
    write "Buzz"
  else
    write i
  end
end
