#!/usr/bin/julia
function value(list)
    result = 0
    for v in list
        result = result * 2
        result = result + v
    end
    return result
end

function find_most(candidates, index)
    count = sum([v[index] for v in candidates])
    return count >= length(candidates) / 2 ? 1 : 0
end

function find_least(candidates, index)
    count = sum([v[index] for v in candidates])
    return count < length(candidates) / 2 ? 1 : 0
end

input_file = ARGS[1]
open(input_file) do io
    data = map.(x -> parse(Int64,x), map(collect, readlines(io)))
    candidates = data
    index = 1
    while length(candidates) > 1
        most = find_most(candidates, index)
        candidates = filter(v -> v[index] == most, candidates)
        index = index + 1
    end
    oxy = value(candidates[1])
    candidates = data
    index = 1
    while length(candidates) > 1
        least = find_least(candidates, index)
        candidates = filter(v -> v[index] == least, candidates)
        index = index + 1
    end
    co2 = value(candidates[1])
    println(oxy * co2)
end
