--- String patterns for use in the loader.
-- TODO: I imagine many of these are inadequate! :( :( :(

--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--
-- [ MIT license: http://www.opensource.org/licenses/mit-license.php ]
--

-- Exports --
local M = {}

--- Optional dot.
M.Dot = "(%.?)"

--- Legal GLSL identifier.
M.Identifier = "[_%a][_%w]*"

--- Zero or more spaces.
M.Spaces = "%s*"

--- Assignment to a variable, struct field, or vector components.
M.Assignment = M.Dot .. M.Spaces .. "(" .. M.Identifier .. ")" .. M.Spaces .. "="

--- Braced substrings.
M.InBraces = "%b{}"

--- Braced substrings.
M.InParens = "%b()"

--- Dummy capture (shader source is zero-terminated) for pattern consistency.
M.Zero = "(%z?)"

--- Function call.
M.Call = M.Zero .. "(" .. M.Identifier .. ")" .. M.Spaces .. M.InParens .. M.Spaces .. "({?)"

--- Function definition.
M.DefineFunc = M.Zero .. "(" .. M.Identifier .. ")" .. M.Spaces .. M.InParens .. M.Spaces .. M.InBraces

--- Struct declaration.
M.Struct = M.Zero .. "struct%s+(" .. M.Identifier .. ")"

--- Variable usage.
M.UseVar = M.Dot .. M.Spaces .. "(" .. M.Identifier .. ")" .. M.Spaces .. "([%({]?)"

--- Semicolon-terminated statement(s).
M.Statement = "[;%)}]?[^;]*;"

--- Variable declaration clause.
M.Declaration = "(" .. M.Identifier .. ")%s+(" .. M.Identifier .. ")" .. M.Spaces .. "[=,;]"

--- Variable initializer clause.
M.Var = "," .. M.Spaces .. "(" .. M.Identifier .. ")" .. M.Spaces .. "[=,;]"

--- Parameter declaration clause.
M.Param = "(" .. M.Identifier .. ")" .. M.Spaces .. "[,%)]"

--- Preprocessor directive.
M.Preprocessor = "#(" .. M.Identifier .. ")([^\n]*)"

--- Function signature.
M.Signature = "(" .. M.Identifier .. ")" .. M.Spaces .. "(" .. M.InParens .. ")" .. M.Spaces .. "(" .. M.InBraces .. ")"

-- Export the module.
return M