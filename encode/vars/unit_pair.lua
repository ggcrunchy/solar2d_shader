--- Data-encoding routines for [0, 1] x [0, 1] pairs.

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
local effect_props = require("corona_shader.effect_props")
local encoding_utils = require("corona_shader.encode.utils")

-- Cached module references --
local _FromXY_
local _VertexDatum_

-- Exports --
local M = {}

--- Adds a unit pair-style parameter to a kernel.
--
-- Some property data is also added for the parameter, cf. @{corona_shader.effect_props.AddVertexProperty}.
-- @ptable kernel Corona shader kernel.
-- @uint index Vertex userdata component index, cf. @{VertexDatum}.
-- @string prop1 Friendly name of number #1...
-- @string prop2 ...and #2.
-- @string combo Friendly name of shader parameter, cf. @{VertexDatum}.
-- @number defx As per @{VertexDatum}.
-- @number defy As per @{VertexDatum}.
function M.AddVertexProperty (kernel, index, prop1, prop2, combo, defx, defy)
	effect_props.AddVertexPropertyState_Datum(kernel, "unit_pair", _VertexDatum_(combo, index, defx, defy), prop1, prop2, combo)
end

--- Encodes two numbers &isin; [0, 1] into a **mediump**-range float for retrieval in GLSL.
-- @number x Number #1...
-- @number y ...and #2.
-- @treturn number Encoded pair.
function M.FromXY (x, y)
	y = y - .5

	return (y < 0 and -1 or 1) * (floor(1023 * x) + abs(y))
end

--- Decodes a **mediump**-range float, assumed to be encoded as per @{FromXY}.
-- @number pair Encoded pair.
-- @treturn number Number #1...
-- @treturn number ...and #2.
function M.ToXY (pair)
	local apair = abs(pair)
	local xpart = floor(apair)

	return xpart / 1023, (pair < 0 and -1 or 1) * (apair - xpart) + .5
end

--- Prepares a unit pair-style parameter for addition to a kernel.
--
-- This parameter should be assigned values encoded as per @{FromXY}.
-- @string name Friendly name of shader parameter.
-- @uint index Vertex userdata component index.
-- @number defx Default number #1, cf. @{FromXY}...
-- @number defy ...and number #2.
-- @treturn table Vertex userdata component.
function M.VertexDatum (name, index, defx, defy)
	return {
		name = name,
		default = _FromXY_(defx, defy),
		min = -1023.5, max = 1023.5,
		index = index
	}
end

-- Register the "unit_pair" property handler.
encoding_utils.DefinePairPropertyHandler{
	name = "unit_pair",
	decode = M.ToXY, encode = M.FromXY,
	min_value = 0, max_value = 1
}

-- Cache module members.
_FromXY_ = M.FromXY
_VertexDatum_ = M.VertexDatum

-- Export the module.
return M