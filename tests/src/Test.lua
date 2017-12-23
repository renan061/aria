local TestCase = require "TestCase"

local Test = {}
Test.__index = Test

function Test.loadFrom(file)
    -- reads a file and returns all of its content
    function read(path)
        local file = assert(io.open(path, "r"))
        local content = file:read("*all")
        file:close()
        return content
    end

    --- splits a string matching the pattern and returns them as an array
    local function split(string, pattern)
        local t = {}
        string:gsub("[^" .. pattern .. "]+", function(x) t[#t + 1] = x end)
        return t
    end

    -- transforms the array of strings into and array of test cases
    local function cases(strs)
        local tests = {}
        local i = 1
        while strs[i] do
            table.insert(
                tests, TestCase.new(strs[i], strs[i + 1], strs[i + 2])
            )
            i = i + 3
        end
        return tests
    end

    local t = {cases = cases(split(read(file), "-"))}
    setmetatable(t, Test)
    return t
end

function Test:run(cmd)
    for _, test in ipairs(self.cases) do
        if not test:run(cmd) then
            return false
        end
    end
    return true
end

function Test:dump()
    for _, test in ipairs(self.cases) do print(test:dump()) end
end

return Test
