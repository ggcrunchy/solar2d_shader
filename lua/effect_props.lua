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
local assert = assert
local format = string.format
local getmetatable = getmetatable
local loaded = package.loaded
local setmetatable = setmetatable
local pairs = pairs

-- Exports --
local M = {}

-- --
local PropertyData = setmetatable({}, { __mode = "k" })

--
local function GetPropertyData (kernel)
	return PropertyData[kernel] or { handlers = {}, hstate = {} }
end

-- --
local Keys = {}

-- --
local Handlers = {}

--- DOCME
function M.AddVertexProperty (kernel, handler_name, vprop, ...)
	assert(not kernel.graph, "Cannot add vertex property to multi-pass kernel")

	local pdata, vdata = GetPropertyData(kernel), kernel.vertexData or {}
	local props = pdata.properties or {
		get = function(t, k, state)
			local v = state[k]

			if v == nil then
				local hstate = pdata.hstate

				for handler in pairs(pdata.handlers) do
					v = handler.get(t, k, state, hstate)

					if v ~= nil then
						return v
					end
				end
			end
		end,

		set = function(t, k, v, state)
			local hstate = pdata.hstate

			for handler in pairs(pdata.handlers) do
				if handler.set(t, k, v, state, hstate) then
					return
				end
			end

			return "none"
		end
	}

	--
	local handler = assert(Handlers[handler_name], "Invalid handler")

	if not pdata.handlers[handler] then
		for i = 1, #handler, 2 do
			pdata.hstate[handler[i]] = handler[i + 1] and {}
		end

		pdata.handlers[handler] = true
	end

	handler.init(pdata.hstate, ...)

	--
	pdata.properties = props

	vdata[#vdata + 1] = vprop

	PropertyData[kernel], kernel.vertexData = pdata, vdata
end

-- --
local Augmented = setmetatable({}, { __mode = "v" })

--
local function GetAccessors (effect, kernel)
	local effect_mt, props = getmetatable(effect), PropertyData[kernel].properties
	local name = format("%s.%s.%s", kernel.category, kernel.group or "custom", kernel.name)

	if props and not Augmented[name] then
		local get, index = props.get, effect_mt.__index
		local set, newindex = props.set, effect_mt.__newindex
		local state = {}

		Augmented[name] = {
			-- Getter --
			get = get and function(t, k, object)
				local v = index(t, k)

				if v ~= nil then
					return v
				else
					return get(t, k, state, object)
				end
			end or index,

			-- Setter --
			set = set and function(t, k, v, object)
				if set(t, k, v, state, object) == "none" then
					newindex(t, k, v)
				end
			end or newindex
		}
	end

	return Augmented[name]
end

-- --
local Proxy = setmetatable({}, { __mode = "k" })

--- DOCME
function M.AugmentEffect (object, kernel)
	local effect, graph = object.fill.effect, kernel.graph

	--
	if graph then
		for k, v in pairs(graph.nodes) do
			local ekernel = assert(effect[k], "Sub-effect not loaded")
			local sub_effect = effect[k]

			if sub_effect and not Proxy[sub_effect] then
				Proxy[sub_effect] = GetAccessors(sub_effect, ekernel)
			end
		end

	--
	elseif not Proxy[effect] then
		Proxy[effect] = GetAccessors(effect, kernel)
	end
end

--- DOCME
function M.DefinePropertyHandler (name, get, set, init, has_prop, keys)
	--
	for i = 1, #(keys or ""), 2 do
		assert(not Keys[keys[i]], "Key already in use")
	end

	--
	local handler = { get = get, set = set, init = init, has_prop = has_prop }

	for i = 1, #(keys or ""), 2 do
		Keys[keys[i]] = keys[i]

		handler[#handler + 1] = keys[i]
		handler[#handler + 1] = not not keys[i + 1]
	end

	Handlers[name] = handler
end

--- DOCME
function M.FoundInProperties (kernel, prefix, prop)
	--
	if prefix then
		kernel = assert(kernel.graph, "Kernel is not multi-pass").nodes[prefix]
	end

	--
	local pdata = PropertyData[kernel]

	if pdata then
		local hstate = pdata.hstate

		for handler in pairs(pdata.handlers) do
			local does_have, min, max = handler.has_prop(hstate, prop)

			if does_have then
				return true, min, max
			end
		end
	end

	return false
end

--
local function GetEffect (object, prefix)
	local effect = object.fill.effect

	if prefix then
		return effect[prefix]
	else
		return effect
	end
end

--- DOCME
function M.GetEffectProperty (object, prefix, prop)
	local effect = GetEffect(object, prefix)
	local proxy = Proxy[effect]

	if proxy then
		return proxy.get(effect, prop, object)
	else
		return effect[prop]
	end
end

--- DOCME
function M.SetEffectProperty (object, prefix, prop, v)
	local effect = GetEffect(object, prefix)
	local proxy = Proxy[effect]

	if proxy then
		proxy.set(effect, prop, v, object)
	else
		effect[prop] = v
	end
end

-- Export the module.
return M