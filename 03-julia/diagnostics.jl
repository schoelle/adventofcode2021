#!/usr/bin/julia
function value(list)
    result = 0
    for v in list
        result = result * 2
        result = result + v
    end
    return result
end

input_file = ARGS[1]
open(input_file) do io
    lines = readlines(io)
    sums = sum(map.(x -> parse(Int64,x),map(collect, lines)))
    half = length(lines) / 2
    bits = map(v -> v > half ? 1 : 0, sums)                    
    gamma = value(bits)
    epsilon = value([1-v for v in bits])
    println(gamma * epsilon)
end
