--- Utilities for parsing GLSL code.

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

-- Standard library imports --
local gsub = string.gsub
local sub = string.sub

-- Modules --
local patterns = require("corona_shader.impl.patterns")

-- Exports --
local M = {}

-- Temporary replacements table --
local RepTable

-- Helper to make a replacement
local function Replace (what)
	return RepTable[sub(what, 2, -2)] or "::MISSING::"
end

--- Performs replacements on the string.
-- @string source Original source.
-- @ptable[opt] replacements If absent, this is a no-op. Otherwise, any occurrence of
-- **"$(KEY)"** in _source_ is replaced by the value associated with key **"KEY"** in this
-- table. Missing values are replaced with **"::MISSING::"**, which is not valid GLSL.
-- @treturn string Source with replacements performed.
function M.InsertReplacements (source, replacements)
	if replacements then
		RepTable = replacements

		source, RepTable = gsub(source, "%$(%b())", Replace)
	end

	return source
end

do
	-- C-style comments replacement
	local function CommentC (before, up_to, comment, after, last)
		local has_up_to = gsub(up_to, patterns.Spaces, "") ~= ""
		local has_after = gsub(after, patterns.Spaces, "") ~= ""

		if has_up_to and has_after then
			local mid = gsub(comment, "[^\n]", "") ~= "" and "\n" or " "

			return before .. up_to .. mid .. after .. last
		elseif has_up_to then
			return before .. up_to .. last
		elseif has_after then
			return before .. after .. last
		else
			return before
		end
	end

	-- C++-style comments replacement
	local function CommentCPP (before, between, last)
		if gsub(between, patterns.Spaces, "") ~= "" then
			return before .. between .. last
		else
			return before
		end
	end

	--- Strips comments from source, to avoid spurious identifiers.
	-- @string source Original source.
	-- @treturn string Source with comments removed.
	function M.StripComments (source)
		source = gsub(source, "(\n?)([^\n]-)/%*(.-)%*/([^\n]*)(\n?)", CommentC)
		source = gsub(source, "(\n?)([^\n]-)//[^\n]*(\n?)", CommentCPP)

		return source
	end
end

-- Export the module.
return M