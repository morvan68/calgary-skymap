function myfloat::init,flo
    if flo ne !null then self.setflo,flo
    return,1
end

function myfloat::getflo
    return,self.flo
end

pro myfloat::setflo,flo
    self.flo=flo
end

pro myfloat__define
    struct={myfloat,flo:0L}
    return
end