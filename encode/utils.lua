--- Data-encoding utilities.

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
local effect_props = require("corona_shader.effect_props")

-- Exports --
local M = {}

--- Defines a property handler for two numbers encoded as a pair.
-- @ptable params Handler parameters. Fields:
--
-- * **name**: Handler name.
-- * **decode**: Decoding routine.
-- * **encode**: Encoding routine.
-- * **min_value**: Minimum value of number #1...
-- * **max_value**: ...and maximum.
-- * **min_value2**: Minimum value of number #2... (If absent, uses **min_value**.)
-- * **max_value2**: ...and maximum. (If absent, uses **max_value**.)
-- * **first_key**: Unique effect state key of "first" field. If absent, auto-generated.
-- * **paired_to_key**: Unique effect state key of "paired to" field. If absent, auto-generated.
function M.DefinePairPropertyHandler (params)
	local decode, encode = params.decode, params.encode
	local min1, max1 = params.min_value, params.max_value
	local min2, max2 = params.min_value2 or min1, params.max_value2 or max1
	local first_key = params.first_key or {}
	local paired_to_key = params.paired_to_key or {}

	effect_props.DefinePropertyHandler(params.name,

		-- Getter --
		function(t, k, _, effect_state)
			local combo, k2 = effect_state[first_key][k], effect_state[paired_to_key][k]

			if k2 then
				local u1, u2 = decode(t[combo or effect_state[first_key][k2]])

				return combo ~= nil and u1 or u2
			end
		end,

		-- Setter --
		function(t, k, v, state, effect_state)
			local k2 = effect_state[paired_to_key][k]

			if k2 then
				state[k] = v

				-- Figure out the names of the second number and the "combined" kernel
				-- parameter. Put the two numbers in order.
				local combo, v2 = effect_state[first_key][k]

				if combo ~= nil then
					v2 = state[k2]
				else
					combo, v, v2 = effect_state[first_key][k2], state[k2], v
				end

				-- If one of the two numbers has yet to be evaluated, decode the default
				-- parameter value. Encode and assign the updated pair.
				if not (v and v2) then
					local u1, u2 = decode(t[combo])

					v, v2 = v or u1, v2 or u2
				end

				t[combo] = encode(v, v2)

				return true
			end
		end,

		-- Initialize --
		function(effect_state, prop1, prop2, combo)
			local paired_to = effect_state[paired_to_key]

			paired_to[prop1], paired_to[prop2], effect_state[first_key][prop1] = prop2, prop1, combo
		end,

		-- Has Property --
		function(effect_state, prop)
			local has_prop = effect_state[paired_to_key][prop]

			if effect_state[first_key][prop] ~= nil then
				return has_prop, min1, max1
			else
				return has_prop, min2, max2
			end
		end,

		-- Keys --
		{ first_key, true, paired_to_key, true }
	)
end

-- Export the module.
return M