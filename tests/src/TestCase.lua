local TestCase = {}
TestCase.__index = TestCase

function TestCase.new(title, input, expected)
    local t = {
        title = title:sub(2, -2),
        input = input:sub(2, -1),
        expected = expected:sub(2, -1)
    }
    setmetatable(t, TestCase)
    return t
end

function TestCase:run(cmd)
    local inputFile = "input.aria"
    os.execute("touch " .. inputFile)

    -- writes the test input to a temporary file
    local file = assert(io.open("input.aria", "w"))
    file:write(self.input)
    file:close()

    -- executes the test command and saves the output
    local stdout = assert(io.popen(cmd .. " " .. inputFile))
    local output = stdout:read("*all")
    stdout:close()

    -- compares the expected output with the actual output
    if output ~= self.expected then
        print("Failed \"" .. self.title .. "\"")
        print("// actual output")
        print(output)
        print("// expected output")
        print(self.expected)
        return false
    end

    return true
end

function TestCase:dump()
    local printf = function(s, ...) io.write(s:format(...)) end
    printf("-- Title:\n%s", self.title)
    printf("-- Input:\n%s", self.input)
    printf("-- Output:\n%s", self.output)
end

return TestCase
