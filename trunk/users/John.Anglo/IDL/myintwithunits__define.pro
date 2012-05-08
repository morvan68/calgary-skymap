function myIntWithUnits::init,value,units
  compile_opt hidden
  if units ne !null then self.setUnits=units
  return,self.myInt::init(value)
end

function myIntWIthUnits::getUnits
  return,self.units
end

pro myIntWithUnits::setUnits,units
  self.units=units
end

pro myIntWithUnits__define
  compile_opt hidden
  struct={myIntWithUnits,units:'',inherits myInt}
end