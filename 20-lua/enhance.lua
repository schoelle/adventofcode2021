#!/usr/bin/lua

function mapchar(b)
   if b then
      return '#'
   else
      return '.'
   end
end

function count_dots(map)
   local n = 0
   if map.bg then
      return math.huge
   end
   for _ in pairs(map.data) do
      n = n + 1
   end
   return n
end

function point2key(x,y)
   return x .. "," .. y
end

function key2point(k)
   return {
      x=tonumber(k:sub(1,k:find(",")-1)),
      y=tonumber(k:sub(k:find(",")+1,#k))
   }
end

function build_lookup(first)
   local res = {}
   for i=1,#first do
      if first:sub(i,i) == "#" then
	 res[i] = true
      end
   end
   return res
end

function build_map(lines)
   local data = {}
   local y = 1
   local width = 0
   for line in lines do
      width=#line
      for x=1,width do
	 local key = point2key(x,y)
	 if line:sub(x,x) == "#" then
	    data[key] = true
	 end
      end
      y = y + 1
   end
   return {data=data,width=width,height=y-1,bg=false}
end

function dot_at(map,x,y)
   if x < 1 or x > map.width or y < 1 or y > map.height then
      return map.bg
   end
   return map.data[point2key(x,y)]
end

function enhanced_value(map,x,y)
   local r = 0
   if dot_at(map,x-1,y-1) then r = r + 256 end
   if dot_at(map,x,y-1) then r = r + 128 end
   if dot_at(map,x+1,y-1) then r = r + 64 end
   if dot_at(map,x-1,y) then r = r + 32 end
   if dot_at(map,x,y) then r = r + 16 end
   if dot_at(map,x+1,y) then r = r + 8 end
   if dot_at(map,x-1,y+1) then r = r + 4 end
   if dot_at(map,x,y+1) then r = r + 2 end
   if dot_at(map,x+1,y+1) then r = r + 1 end
   return r+1
end

function enhanced_map(map,lookup)
   local new_width=map.width+2
   local new_height=map.height+2
   local new_bg = false
   if map.bg then
      new_bg = lookup[512]
   else
      new_bg = lookup[1]
   end
   new_data = {}
   for y=1,new_width do
      for x=1,new_height do
	 local v = enhanced_value(map,x-1,y-1)
	 if lookup[v] then
	    new_data[point2key(x,y)] = true
	 end
      end
   end
   return {data=new_data,width=new_width,height=new_height,bg=new_bg}
end

function first_problem(input)
   local map1 = enhanced_map(input.map, input.lookup)
   local map2 = enhanced_map(map1, input.lookup)
   print("Problem 1: " .. count_dots(map2))
end

function second_problem(input)
   local map=input.map
   for i=1,50 do
      map = enhanced_map(map, input.lookup)
   end
   print("Problem 2: " .. count_dots(map))
end

function read_input(filename)   
   lines =  io.lines(arg[1])
   first = lines()
   lookup = build_lookup(first)
   lines() -- Skip the empty line
   map = build_map(lines)
   return {lookup=lookup,map=map}
end

input = read_input(arg[1])
first_problem(input)
second_problem(input)
