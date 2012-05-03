function dot0d, vec1, vec2
  result=0
  for indx=0,2 do begin
    for ind=0,98 do result = result + vec1[indx,ind] * vec2[indx,ind]
  end
  return, result
end

function dot1d, vec1, vec2
  result=0
  for indx=0,2 do result = result + vec1[indx,*] * vec2[indx,*]
  return, result
end