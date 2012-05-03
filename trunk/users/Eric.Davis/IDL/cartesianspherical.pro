function spherical,x,y,z
  r= double((x^2+y^2+z^2)^(0.5))
  phi = double(atan(y/x))
  theta= double(acos(z/r))
 return, [double(r),double(phi),double(theta)]
end


function cartesian,a
r=a(0)
phi=a(1)
theta=a(2)
x= double(r*cos(phi)*sin(theta))
y= double(r*sin(phi)*sin(theta))
z= double(r*cos(theta))
return, [double(x),double(y),double(z)]
end


f= double(randomu(seeds))
g= double(randomu(seeds))
h= double(randomu(seeds))


result= spherical(f,g,h)

result2= cartesian(result)

if (result2(0)-f) lt 1e-10 then print, "x =",result2(0) else print, "Something went wrong, here are the initial and final x coordinates:", f, result2(0)

if (result2(1)-g) lt 1e-10 then print, "y =",result2(1) else print, "Something went wrong, here are the initial and final y coordinates:", g, result2(1)

if (result2(2)-h) lt 1e-10 then print, "z =",result2(2) else print, "Something went wrong, here are the initial and final z coordinates:", h, result2(2)

end