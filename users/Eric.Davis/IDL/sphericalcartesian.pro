function spherical,a
  x=a(0)
  y=a(1)
  z=a(2)
  r= double((x^2+y^2+z^2)^(0.5))
  phi = double(atan(y,x))
  theta= double(acos((z/r)))
 return, [r,phi,theta]
end


function cartesian,r,phi,theta
  x= double(r*cos(phi)*sin(theta))
  y= double(r*sin(phi)*sin(theta))
  z= double(r*cos(theta))
    print, "x =", x, "     y =", y, "     z =", z
  return, [x,y,z]
end


f= abs(double(randomn(seeds)))
g= abs(double(randomn(seeds)))
h= abs(double(randomn(seeds)))


result= cartesian(f,g,h)

result2= spherical(result)

if (result2(0)-f) lt 1e-10 then print, "r =",result2(0) else print, "Something went wrong, here are the initial and final r values", f, result2(0)

if (result2(1)-g) lt 1e-10 then print, "phi =",result2(1), " radians" else print, "Something went wrong, here are the initial and final phi values:", g, result2(1)

if (result2(2)-h) lt 1e-10 then print, "theta =",result2(2), " radians" else print, "Something went wrong, here are the initial and final theta values:", h, result2(2)

end