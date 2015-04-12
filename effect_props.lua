--- Helpers for getting and setting effect properties.
--
-- Such properties may be raw parameters. The motivation behind this module, however, is that
-- it is sometimes more convenient to get or set parameters indirectly, say when a more user-
-- friendly form is available (e.g. degrees vs. radians or pre-evaluated cosine and sine) or
-- when multiple values are encoded into one value.

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

-- Cached module references --
local _AddVertexPropertyState_

-- Exports --
local M = {}

-- Kernel -> property data map --
local PropertyData = setmetatable({}, { __mode = "k" })

-- Lazily looks up a kernel's property data
local function GetPropertyData (kernel)
	return PropertyData[kernel] or { handlers = {}, hstate = {} }
end

-- Set of keys reserved by property handlers (to enforce uniqueness) --
local Keys = {}

-- Name -> handler map --
local Handlers = {}

--- Adds effect property state to a (vertex userdata-based) kernel.
-- @ptable kernel Corona shader kernel.
-- @param handler_name Name of handler, as defined by @{DefinePropertyHandler}, which the
-- new state is assumed to use.
--
-- If this is the first association of this handler to _kernel_, its **get** and **set**
-- routines are added to _kernel_'s list and its keys are initialized in the handler state.
--
-- The handler's **init** logic is then called as `init(hstate, ...)`, where _hstate_ is the
-- handler state. This can be used, say, to assign all the data needed by some properties.
-- @param ... Initialization arguments.
function M.AddVertexPropertyState (kernel, handler_name, ...)
	assert(not kernel.graph, "Cannot add vertex property state to multi-pass kernel")
	-- TODO: Uniform data? (Actually, as is, could be general-purpose...)

	-- If this is the first property, configure the property data and ensure its registration.
	local pdata = GetPropertyData(kernel)
	local handlers, hstate = pdata.handlers, pdata.hstate -- capture these rather than pdata itself
	local props = pdata.properties or {
		get = function(t, k, state)
			local v = state[k]

			if v == nil then
				for handler in pairs(handlers) do
					v = handler.get(t, k, state, hstate)

					if v ~= nil then
						return v
					end
				end
			end
		end,

		set = function(t, k, v, state)
			for handler in pairs(handlers) do
				if handler.set(t, k, v, state, hstate) then
					return
				end
			end

			return "none"
		end
	}

	PropertyData[kernel], pdata.properties = pdata, props

	-- If this is the first time this handler has been added to the kernel, register the
	-- handler and add the keys to the state. Initialize the property state.
	local prop_handler = assert(Handlers[handler_name], "Invalid handler")

	if not handlers[prop_handler] then
		for i = 1, #prop_handler, 2 do
			hstate[prop_handler[i]] = prop_handler[i + 1] and {}
		end

		handlers[prop_handler] = true
	end

	prop_handler.init(pdata.hstate, ...)
end

--- Variant of @{AddVertexPropertyState} that takes a vertex datum argument.
-- @ptable kernel Corona shader kernel.
-- @param handler_name As per @{AddVertexPropertyState}.
-- @ptable vertex_datum Vertex userdata component, ostensibly associated with the new state,
-- that gets added to _kernel_.**vertexData** (which is first created, if necessary).
--
-- This is merely for convenience, as it keeps parameter definition and state addition
-- together in the calling code.
-- @param ... Initialization arguments.
function M.AddVertexPropertyState_Datum (kernel, handler_name, vertex_datum, ...)
	local vdata = kernel.vertexData or {}

	vdata[#vdata + 1] = vertex_datum

	kernel.vertexData = vdata

	_AddVertexPropertyState_(kernel, handler_name, ...)
end

-- Full name -> property accessors map --
local Augmented = setmetatable({}, { __mode = "v" })

-- Effect -> state map --
local State = setmetatable({}, { __mode = "k" })

--
local function GetAccessors (effect, kernel)
	local effect_mt, props = getmetatable(effect), PropertyData[kernel].properties
	local name = format("%s.%s.%s", kernel.category, kernel.group or "custom", kernel.name)

	if props and not Augmented[name] then
		local get, index = props.get, effect_mt.__index
		local set, newindex = props.set, effect_mt.__newindex

		State[effect] = {}

		Augmented[name] = {
			-- Getter --
			get = get and function(t, k, object)
				local v = index(t, k)

				if v ~= nil then
					return v
				else
					return get(t, k, State[effect], object)
				end
			end or index,

			-- Setter --
			set = set and function(t, k, v, object)
				if set(t, k, v, State[effect], object) == "none" then
					newindex(t, k, v)
				end
			end or newindex
		}
	end

	return Augmented[name]
end

-- Fill effect -> proxy map --
local Proxy = setmetatable({}, { __mode = "k" })

--- DOCME
-- @pobject object
-- @ptable kernel
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
-- @param name
-- @callable get
-- @callable set
-- @callable init
-- @callable has_prop
-- @ptable[opt] keys
function M.DefinePropertyHandler (name, get, set, init, has_prop, keys)
	assert(not Handlers[name], "Property handler name already in use")

	--
	for i = 1, #(keys or ""), 2 do
		assert(not Keys[keys[i]], "Key already in use")
	end

	--
	local handler = { get = get, set = set, init = init, has_prop = has_prop }

	for i = 1, #(keys or ""), 2 do
		Keys[keys[i]] = true

		handler[#handler + 1] = keys[i]
		handler[#handler + 1] = not not keys[i + 1]
	end

	Handlers[name] = handler
end

--- DOCME
-- @ptable kernel
-- @string[opt] prefix
-- @string prop
-- @treturn boolean X
-- @treturn ?number Y
-- @treturn ?number Z
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

-- Prefix-aware helper to resolve effect
local function GetEffect (object, prefix)
	local effect = object.fill.effect

	if prefix then
		return effect[prefix]
	else
		return effect
	end
end

--- DOCME
-- @pobject object
-- @string[opt] prefix
-- @string prop
-- @return V
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
-- @pobject object
-- @string[opt] prefix
-- @string prop
-- @param v
function M.SetEffectProperty (object, prefix, prop, v)
	local effect = GetEffect(object, prefix)
	local proxy = Proxy[effect]

	if proxy then
		proxy.set(effect, prop, v, object)
	else
		effect[prop] = v
	end
end

-- Cache module members.
_AddVertexPropertyState_ = M.AddVertexPropertyState

-- Export the module.
return M