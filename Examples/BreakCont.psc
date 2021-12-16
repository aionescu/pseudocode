let a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

for i = 0 to length a - 1 do
  if a[i] > 5 then
    write a[i]
    break
  end
end

for i = 0 to length a - 1 do
  if a[i] < 7 then
    continue
  end

  write a[i]
  break
end
