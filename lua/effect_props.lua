--- Helpers for getting and setting effect properties.

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
local getmetatable = getmetatable
local setmetatable = setmetatable

-- Modules --
local pack = require("corona_shader.lua.pack")

-- Cookies --
local _general = {}

-- Exports --
local M = {}

-- --
local PropertyData = setmetatable({}, { __mode = "k" })

--
local function GetPropertyData (kernel)
	return PropertyData[kernel] or { paired_to = {}, first = {} }
end

--- DOCME
function M.AddProperty_UnitPairVertex (kernel, index, prop1, prop2, combo, a, b)
	local kdata, vdata = GetPropertyData(kernel), kernel.vertexData or {}
	local props = kdata.properties or {
		id = ("%s.%s"):format(kernel.category, kernel.name), needs_state = true,

		__index = function(t, k, state)
			local v = state[k]

			if v == nil then
				local combo, k2 = kdata.first[k], kdata.paired_to[k]

				if k2 then
					local u1, u2 = pack.UnitPair_XY(t[combo or kdata.first[k2]])

					return combo and u1 or u2
				end
			end
		end,

		__newindex = function(t, k, v, state)
			local k2 = kdata.paired_to[k]

			if k2 then
				state[k] = v

				--
				local combo, v2 = kdata.first[k]

				if combo then
					v2 = state[k2]
				else
					combo, v, v2 = kdata.first[k2], state[k2], v
				end

				--
				if not (v and v2) then
					local u1, u2 = pack.UnitPair_XY(t[combo])

					v, v2 = v or u1, v2 or u2
				end

				t[combo] = pack.XY_UnitPair(v, v2)
			else
				return "none"
			end
		end
	}

	--
	kdata.paired_to[prop1], kdata.paired_to[prop2], kdata.properties = prop2, prop1, props

	vdata[#vdata + 1] = pack.VertexDatum_UnitPair(combo, index, a, b)

	PropertyData[kernel], kernel.vertexData, kdata.first[prop1] = kdata, vdata, combo
end

-- --
local AugmentedMT = setmetatable({}, { __mode = "k" })

--
local function GetID (props)
	if props and props.id ~= nil then
		return props.id
	else
		return _general
	end
end

--
local function GetMT (effect, kernel)
	local effect_mt, props = getmetatable(effect), PropertyData[kernel].properties
	local id, set = GetID(props), AugmentedMT[effect_mt]

	if props and not (set and set[id]) then
		set = set or {}

		local old_index, old_newindex = effect_mt.__index, effect_mt.__newindex
		local new_index, new_newindex = props.__index, props.__newindex
		local state = props.needs_state and {}

		set[id] = {
			-- __index metamethod --
			__index = new_index and function(t, k)
				local v = old_index(t, k)

				if v ~= nil then
					return v
				else
					return new_index(t, k, state)
				end
			end or old_index,

			-- __newindex metamethod --
			__newindex = new_newindex and function(t, k, v)
				if new_newindex(t, k, v, state) == "none" then
					old_newindex(t, k, v)
				end
			end or old_newindex
		}

		AugmentedMT[effect_mt] = set
	end

	return set and set[id]
end

-- --
local Proxy = setmetatable({}, { __mode = "k" })

--- DOCME
function M.AugmentEffect (object, kernel)
	local effect = object.fill.effect

	if not Proxy[effect] then
		local mt = GetMT(effect, kernel)

		if mt then
			Proxy[effect] = mt
		end
	end
end

--- DOCME
function M.FoundInProperties (kernel, prop)
	local kdata = PropertyData[kernel] -- TODO: Assumes [0, 1]...

	return (kdata and kdata.paired_to[prop]) ~= nil
end

--- DOCME
function M.GetEffectProperty (object, prefix, prop)
	local effect = object.fill.effect
	local proxy = Proxy[effect]

	if prefix then
		return effect[prefix][prop] -- todo: if proxied?
	elseif proxy then
		return proxy.__index(effect, prop)
	else
		return effect[prop]
	end
end

--- DOCME
function M.SetEffectProperty (object, prefix, prop, v)
	local effect = object.fill.effect
	local proxy = Proxy[effect]

	if prefix then
		effect[prefix][prop] = v -- todo: if proxied?
	elseif proxy then
		proxy.__newindex(effect, prop, v)
	else
		effect[prop] = v
	end
end

-- Export the module.
return M