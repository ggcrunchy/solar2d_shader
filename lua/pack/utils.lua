--- Utilities for data-packing routines.

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

-- Modules --
local effect_props = require("corona_shader.lua.effect_props")

-- Exports --
local M = {}

--- DOCME
function M.DefinePairPropertyHandler (name, to_xy, from_xy, min_value, max_value, first_key, paired_to_key)
	first_key = first_key or {}
	paired_to_key = paired_to_key or {}

	effect_props.DefinePropertyHandler(name,

		-- Getter --
		function(t, k, _, hstate)
			local combo, k2 = hstate[first_key][k], hstate[paired_to_key][k]

			if k2 then
				local u1, u2 = to_xy(t[combo or hstate[first_key][k2]])

				return combo and u1 or u2
			end
		end,

		-- Setter --
		function(t, k, v, state, hstate)
			local k2 = hstate[paired_to_key][k]

			if k2 then
				state[k] = v

				--
				local combo, v2 = hstate[first_key][k]

				if combo then
					v2 = state[k2]
				else
					combo, v, v2 = hstate[first_key][k2], state[k2], v
				end

				--
				if not (v and v2) then
					local u1, u2 = to_xy(t[combo])

					v, v2 = v or u1, v2 or u2
				end

				t[combo] = from_xy(v, v2)

				return true
			end
		end,

		-- Initialize --
		function(hstate, prop1, prop2, combo)
			hstate[paired_to_key][prop1], hstate[paired_to_key][prop2], hstate[first_key][prop1] = prop2, prop1, combo
		end,

		-- Has Property --
		function(hstate, prop)
			return hstate[paired_to_key][prop], min_value, max_value
		end,

		-- Keys --
		{ first_key, true, paired_to_key, true }
	)
end

-- Export the module.
return M