function myfloatwithunits::init,flo,units
    self.set,flo,units
    return,1
end

function myfloatwithunits::get
    return,string(self.flo,self.units)
end


pro myfloatwithunits::set,flo,units
    if self.flo ne !null then self.flo=flo 
    if self.units ne !null then self.units=units
end
    
pro myfloatwithunits__define
    struct={myfloatwithunits    $
            ,inherits myfloat   $
            ,units:''}
    return
end