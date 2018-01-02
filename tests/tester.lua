-- lists all files named *.at within the given directory
function listTests(directory)
    local tests = {}
    local pipe = assert(io.popen("ls " .. directory .. "/*.at"))
    for filename in pipe:lines() do
        table.insert(tests, filename)
    end
    pipe:close()
    return tests
end

-- cases {
--     "title": {
--         input:  "... input  ...",
--         output: "... output ...",
--     },
--     ...
-- }
function readTestCases(path)
    -- auxiliary
    local testDivider = string.rep("-", 80)
    local titleDivider = "-- Title"
    local inputDivider = "-- Input"
    local outputDivider = "-- Output"

    -- cases
    local file = assert(io.open(path, "r"))
    local cases = {}

    assert(file:read("*line") == testDivider, "expecting test divider")
    for line in file:lines() do
        assert(line == titleDivider, "expecting title divider")
        -- title
        local title = file:read("*line")
        cases[title] = {}
        -- input
        assert(file:read("*line") == inputDivider, "expecting input divider")
        cases[title]["input"] = ""
        for ln in file:lines() do
            if ln == outputDivider then break end
            cases[title]["input"] = cases[title]["input"] .. ln .. "\n"
        end
        -- output
        cases[title]["output"] = ""
        for ln in file:lines() do
            if ln == testDivider then break end
            cases[title]["output"] = cases[title]["output"] .. ln .. "\n"
        end
    end

    file:close()
    return cases
end

function runTest(cmd, test)
    local inputFile = "input.aria"
    os.execute("touch " .. inputFile)
    -- writes the test input to a temporary file
    local file = assert(io.open("input.aria", "w"))
    file:write(test["input"])
    file:close()

    -- executes the test command and saves the output
    local stdout = assert(io.popen(cmd .. " " .. inputFile .. " 2>&1"))
    local output = stdout:read("*all"):gsub("\t", "    ") -- TODO: remove \t sub
    stdout:close()

    test["output"] = test["output"]:sub(1, -2) -- avoids the last \n issue

    -- compares the expected output with the actual output
    local ok = output == test["output"]
    if not ok then
        io.write("// expected\n")
        io.write("[")
        io.write(test["output"])
        io.write("]")
        io.write("\n// got\n")
        io.write("[")
        io.write(output)
        io.write("]")
    end

    os.execute("rm -f " .. inputFile)
    return ok
end

-- main
local directory = assert(arg[1], "must provide a directory")
local binary = assert(arg[2], "must provide a binary")

for _, test in ipairs(listTests(directory)) do
    local testCases = readTestCases(test)
    for title, case in pairs(testCases) do
        if not runTest(binary, case) then
            io.write("\nIn \"" .. test .. "\" failed [" .. title .. "]\n")
            return false
        end
    end
end

io.write("Ok " .. directory .. "\n")
return true
