
function cro,f1,f2,f3,g1,g2,g3
          x=f2*g3-f3*g2
          y=-(f1*g3-f3*g1)
          z=f1*g2-f2*g1
      dot=f1*g1+f2*g2+f3*g3
      absv1=sqrt(f1*f1+f2*f2+f3*f3)
      absv2=sqrt(g1*g1+g2*g2+g3*g3)
      theta=acos(dot/((absv1)*(absv2)))
          co=cos(theta)
          si=sin(theta)
          a=x/sqrt(x^2+y^2+z^2)
          b=y/sqrt(x^2+y^2+z^2)
          c=z/sqrt(x^2+y^2+z^2)
          normres=[a,b,c]
      row1=[co+(1-co)*a^2,a*b*(1-co)-c*si,b*si+a*c*(1-co)]
      row2=[c*si+a*b*(1-co),co+(1-co)*b^2,-a*si+b*c*(1-co)]
      row3=[-b*si+a*c*(1-co),a*si+b*c*(1-co),co+(1-co)*c^2]
   print,'The rotation axis is given by:',normres,'   theta in radians:',theta
   print,'The rotation matrix is given by:'
   print,row1
   print,row2
   print,row3
end


f=double([1,0,1])
g=double([0,sqrt(2),0])

f1=f(0)
f2=f(1)
f3=f(2)
g1=g(0)
g2=g(1)
g3=g(2)

if abs(sqrt(f(0)^2+f(1)^2+f(2)^2)-sqrt(g(0)^2+g(1)^2+g(2)^2)) lt 1e-2 then test=cro(f1,f2,f3,g1,g2,g3) else print,'These vectors are not a rotation of one another'


end