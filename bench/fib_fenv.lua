
function fib(n)
   env = _ENV
   _ENV = env
   if n < 2 then
      return n
   else
      return fib(n-1) + fib(n-2)
   end
end

for i=0,1000 do
   fib(10)
end

start_time=os.clock()
for i=0,500000 do
   fib(10)
end
print("cpu time: ".. (os.clock()-start_time)*1000)
