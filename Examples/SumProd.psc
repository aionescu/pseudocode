function sum(l: [Float]): Float
  let sum = 0.0

  for i = 0 to length l - 1 do
    sum = sum + l[i]
  end

  return sum
end

function prod(l: [Float]): Float
  let prod = 1.0

  for i = 0 to length l - 1 do
    prod = prod * l[i]
  end

  return prod
end

program
  let n: Float = read "n: "
  let l: [Float] = []

  let i = 1.0

  while i <= n do
    push l, i
    i = i + 0.5
  end

  write sum(l)
  write prod(l)
end
