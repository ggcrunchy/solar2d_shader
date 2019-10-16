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
local gmatch = string.gmatch
local gsub = string.gsub
local match = string.match
local pairs = pairs
local sub = string.sub
local type = type

-- Modules --
local patterns = require("corona_shader.impl.patterns")

-- Cached module references --
local _StripComments_

-- Exports --
local M = {}

--
--
--

-- Local tag block --
local TagBlockPattern = "%$(" .. patterns.InBraces .. ")"

-- Local tag definition --
local TagDefinitionPattern = "(" .. patterns.Identifier .. ")" .. patterns.Spaces .. "=" .. patterns.Spaces .. "(%w+)"

-- Tag to replace --
local ReplacePattern = "%$(" .. patterns.InParens .. ")"

-- Temporary replacements table --
local RepTable

-- Helper to make a (possibly recursive) replacement
local function Replace (what)
	return RepTable[sub(what, 2, -2)] or "::MISSING::"
end

--- Performs replacements on the string.
-- @string source Original source.
-- @ptable[opt] replacements If absent, this is a no-op. Otherwise, any occurrence of
-- **"$(KEY)"** in _source_ is replaced by the value associated with key **"KEY"** in this
-- table. Missing values are replaced with **"::MISSING::"** (which is invalid GLSL).
--
-- Keys may be nested, e.g. as **"$(KEY_$(ID))"**.
--
-- Additionally, for any string of the form **"${KEY=VALUE}"** (with spaces allowed) found in
-- _source_, **"VALUE"** will be added temporarily to _replacements_ under key **"KEY"**. The
-- string is then removed from _source_. This step occurs before replacements.
-- @treturn string Source with replacements performed.
function M.InsertReplacements (source, replacements)
	if type(replacements) == "table" then
		-- Bring in the replacements, stripping any comments to ease parsing.
		RepTable = {}

		for k, v in pairs(replacements) do
			RepTable[k] = _StripComments_(v)
		end

		-- Resolve any tag definitions, then strip them from the source.
		for tag_block in gmatch(source, TagBlockPattern) do
			local tag, rep = match(tag_block, TagDefinitionPattern)

			if tag then
				RepTable[tag] = rep
			end
		end

		source = gsub(source, TagBlockPattern, "")

		-- Replace any tags. Make multiple passes, in case a replacement introduces tags.
		-- With that done, throw away the temporary replacements table.
		repeat
			local was = source

			source = gsub(source, ReplacePattern, Replace)
		until source == was

		RepTable = nil
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

_StripComments_ = M.StripComments

return M