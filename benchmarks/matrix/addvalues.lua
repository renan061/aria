
input = assert(io.open("matrix.aria", "r"))
tocopy = input:read("*all")
input:close()

function writeMatrix(output, n, name)
    output:write("value " .. name .. " = Immutable [\n")
    for i = 1, n do
        output:write("\tImmutable [")
        for j = 1, n do
            output:write(math.random() * 1000)
            if j == n then
                output:write("]")
            end
            if i ~= n or j ~= n then
                output:write(", ")
            end
        end
        output:write("\n")
    end
    output:write("];\n")
end

function writeArray(output, n, name)
    output:write("value " .. name .. " = Immutable [")
    for i = 1, n do
        output:write(math.random() * 1000)
        if i ~= n then
            output:write(", ")
        end
    end
    output:write("];\n")
end

-- changing matrixes' size
n = 80
tocopy = tocopy:gsub("value n = %d+;", "value n = " .. n ..";" )

-- prepending matrixes A and B
local output = assert(io.open("test.aria", "w"))
writeMatrix(output, n, "a")
writeArray(output, n, "b")
output:write(tocopy)
output:close()
