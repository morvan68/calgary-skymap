
function rotatev,v,theta
      n=[0,0,1]
      nnorm=n/sqrt(n(0)^2+n(1)^2+n(2)^2)
      vpar=nnorm*(nnorm(0)*v(0)+nnorm(1)*v(1)+nnorm(2)*v(2))
      vperp=v-vpar
      u=crossp(n,v)
      vperprot=vperp*cos(theta)+u*sin(theta)
      vfinal=vpar+vperprot
      print,vfinal
end


v=[1,2,3]
theta=!pi/3

test=rotatev(v,theta)

end