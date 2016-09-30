

function ack(env, m, n)
   setfenv(1, env)
   if m == 0 then
      return n+1
   elseif n == 0 then
      return ack(env, m-1, 1)
   else
      return ack(env, m-1, ack(env, m, n-1))
   end
end

start_time=os.clock()
for i=0,5 do
   ack(getfenv(1), 3,9)
end
print("cpu time: ".. (os.clock()-start_time)*1000)
      
