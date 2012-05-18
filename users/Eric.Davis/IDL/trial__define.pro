function trial::init,value
  if value eq !null then value=1
  self.value=value
  self.value=(self.value)*(self.value)
  print,'this is the square of the scalar you entered',self.value
  return,value
end

pro trial::init,value
  self.value=value
  if value eq 1 then print, 'yee' else print, (value+10)^2
  return
end
  
  
pro trial__define
  s={trial,value:0l}
  return
end