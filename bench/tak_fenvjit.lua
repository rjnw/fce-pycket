
function tak(x,y,z)
   setfenv(1, getfenv(1))
   if y < x then
      return tak(tak(x-1, y, z),
                 tak(y-1, z, x),
                 tak(z-1, x, y))
   else
      return z
   end
end

start_time=os.clock()
for i=0,500 do
   tak(18,12,6)
end
print("cpu time: ".. (os.clock()-start_time)*1000)
