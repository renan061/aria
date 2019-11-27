
local input = assert(io.open("source.aria", "r"))
local tocopy = input:read("*all")
input:close()

local function writeMatrix(output, n, name)
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

-- changing matrixes' size
local n = 500
tocopy = tocopy:gsub("value n1 = %d+;", "value n1 = " .. n ..";" )

-- prepending matrixes A and B
local output = assert(io.open("test.aria", "w"))
writeMatrix(output, n, "a")
writeMatrix(output, n, "b")
output:write(tocopy)
output:close()
