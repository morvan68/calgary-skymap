function myInt::init, value
  compile_opt hidden
  if value ne !null then self.setValue,value
  return,1
end

function myInt::getValue
  return,self.value
end

pro myInt::setValue,value
  self.value=value
end

pro myInt__define
  compile_opt hidden
  struct={myInt,value:0l}
end