let a = [0, 0, 0, 0, 0]

for i = 0 to length a - 1 do
  write "Enter a[", i, "]: " -- a
  a[i] = read ""
  a[i] = a[i] * a[i]
end

for i = 0 to length a - 1 do {- aa -}
  write "a[", i, "] = ", a[i]
end
