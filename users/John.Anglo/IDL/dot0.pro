function dot0, vec1, vec2
  result=0
  for indx=0,2 do result = result + vec1[indx] * vec2[indx]
  return, result
end