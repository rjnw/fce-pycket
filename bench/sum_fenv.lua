
function sum(n)
   env = _ENV
   _ENV = env
   total = 0
   for i=0,n do
      total = total + i
   end
   return total
end

start_time=os.clock()
for i=0,10000 do
   sum(10000)
end
print("cpu time: ".. (os.clock()-start_time)*1000)

