program
  let a = [0, 0, 0, 0, 0]

  for i = 0 to length a - 1 do
    write "Enter a[", i, "]: "
    a[i] = read ""
    a[i] = a[i] * a[i]
  end

  for i = length a - 1 down to 0 do
    write "a[", i, "] = ", a[i]
  end
end
