--- Lua-side data-packing routines.

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

-- Cached module references --
local _XY_UnitPair_

-- Exports --
local M = {}

--- DOCME
function M.UnitPair_XY (pair)
	local apair = abs(pair)
	local xpart = floor(apair)

	return xpart / 1023, (pair < 0 and -1 or 1) * (apair - xpart) + .5
end

--- DOCME
function M.VertexDatum_UnitPair (name, index, defx, defy)
	return {
		name = name,
		default = _XY_UnitPair_(defx, defy),
		min = -1023.5, max = 1023.5,
		index = index
	}
end

--- DOCME
-- @number x A value &isin; [0, 1]...
-- @number y ...and another one.
-- @treturn number The packed value.
function M.XY_UnitPair (x, y)
	y = y - .5

	return (y < 0 and -1 or 1) * (floor(1023 * x) + abs(y))
end

-- triple, quad...

-- Todo: [0, 1], [0, 1)
-- Integer in [0, 1023], [0, 1); [0, 1023], [0, 1]
-- Sign meant for other purpose: need to increment integer to guard against 0, limits range to [0, 1022]
-- Barycentric coordinates (not really a packing, more a helper... perhaps elsewhere?)
-- Two integers in [0, 511]... or four...
-- Quantized to lattice of size 1023, with dimensions [0, 2^m), [0, 2^n), s.t. n + m = 10
-- Vector versions (unpack side)

-- Cache module members.
_XY_UnitPair_ = M.XY_UnitPair

-- Export the module.
return M