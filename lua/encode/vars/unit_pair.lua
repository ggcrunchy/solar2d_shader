--- Lua-side data-packing routines for [0, 1] x [0, 1] pairs.

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
local abs = math.abs
local floor = math.floor

-- Modules --
local effect_props = require("corona_shader.lua.effect_props")
local pack_utils = require("corona_shader.lua.encode.utils")

-- Cached module references --
local _FromXY_
local _VertexDatum_

-- Exports --
local M = {}

--- DOCME
function M.AddVertexProperty (kernel, index, prop1, prop2, combo, a, b)
	effect_props.AddVertexProperty(kernel, "unit_pair", _VertexDatum_(combo, index, a, b), prop1, prop2, combo)
end

--- DOCME
-- @number x A value &isin; [0, 1]...
-- @number y ...and another one.
-- @treturn number The packed value.
function M.FromXY (x, y)
	y = y - .5

	return (y < 0 and -1 or 1) * (floor(1023 * x) + abs(y))
end

--- DOCME
function M.ToXY (pair)
	local apair = abs(pair)
	local xpart = floor(apair)

	return xpart / 1023, (pair < 0 and -1 or 1) * (apair - xpart) + .5
end

--- DOCME
function M.VertexDatum (name, index, defx, defy)
	return {
		name = name,
		default = _FromXY_(defx, defy),
		min = -1023.5, max = 1023.5,
		index = index
	}
end

--
pack_utils.DefinePairPropertyHandler("unit_pair", M.ToXY, M.FromXY, 0, 1)

-- Cache module members.
_FromXY_ = M.FromXY
_VertexDatum_ = M.VertexDatum

-- Export the module.
return M