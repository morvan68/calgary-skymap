
function euler::init,theta,vec
      self.theta=theta
      self.vec=vec
    print,self.rot(theta,vec)
    return,1
end

function euler::rot,theta,vec
      st=systime(1)
;# defining an axis to rotate around, where nhat is the normalized rotation axis
      n = [0,0,1]
      norm = ((n(0))^2+(n(1))^2+(n(2))^2)^(0.5)
      nhat = n/norm
      print,nhat
;#  Now defining the constansts a, b, c and any other ones used in the rotation matrix
      a=nhat(0)
      b=nhat(1)
      c=nhat(2)
      x=vec(0)
      y=vec(1)
      z=vec(2)
      co=cos(theta)
      si=sin(theta)
;#and now the rotation 
      row1= [(a^2+(1-a^2)*co),(a*b*(1-co)-c*si),(a*c*(1-co)+b*si)]
      row2= [(a*b*(1-co)+c*si),(b^2+(1-b^2)*co),(b*c*(1-co)-a*si)]
      row3= [(a*c*(1-co)-b*si),(b*c*(1-co)+a*si),(c^2+(1-c^2)*co)]
      v1=row1(0)*x+row1(1)*y+row1(2)*z
      v2=row2(0)*x+row2(1)*y+row2(2)*z
      v3=row3(0)*x+row3(1)*y+row3(2)*z
      norman=sqrt((v1)^2+(v2)^2+(v3)^2)
      vector=[v1,v2,v3]
      normalized=vector/norman
    return,[[row1],[row2],[row3],[vector]]

end


function euler::get
    return,string(self.vec,self.theta)
end




pro euler__define
    struct={euler         $
            ,theta:0D     $
            ,vec:fltarr(3)}
    return
end