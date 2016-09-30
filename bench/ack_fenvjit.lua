

function ack(m, n)
   setfenv(1, getfenv(1))
   if m == 0 then
      return n+1
   elseif n == 0 then
      return ack(m-1, 1)
   else
      return ack(m-1, ack(m, n-1))
   end
end

start_time=os.clock()
for i=0,10 do
   ack(3,9)
end
print("cpu time: ".. (os.clock()-start_time)*1000)
      
