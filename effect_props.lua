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
local find = string.find
local format = string.format
local getmetatable = getmetatable
local setmetatable = setmetatable
local pairs = pairs
local sub = string.sub

-- Cached module references --
local _AddPropertyState_
local _FoundInProperties_Parsed_
local _GetEffectProperty_
local _GetEffectProperty_Parsed_
local _GetName_
local _ParseProperty_
local _SetEffectProperty_
local _SetEffectProperty_Parsed_

-- Exports --
local M = {}

-- Name -> subeffects map --
local MultiPass = {}

--- Prepares a multi-pass kernel for later use in @{AssignEffect}.
-- @ptable kernel Corona shader kernel.
function M.AddMultiPassEffect (kernel)
	local graph = assert(kernel.graph, "Not a multi-pass kernel")
	local name = _GetName_(kernel)

	if not MultiPass[name] then
		local sub_effects = {}

		for k, v in pairs(graph.nodes) do
			sub_effects[k] = v.effect
		end

		MultiPass[name] = sub_effects
	end
end

-- Set of keys reserved by property handlers (to enforce uniqueness) --
local Keys = {}

-- Name -> handler map --
local Handlers = {}

-- Name -> property data map --
local PropertyData = {}

--- Adds effect property state to a kernel.
-- @ptable kernel Corona shader kernel.
-- @param handler_name Name of handler, as defined by @{DefinePropertyHandler}, which the
-- new state is assumed to use.
--
-- If this is the first association of this handler to _kernel_, its **get** and **set**
-- routines are added to _kernel_'s list and its keys are initialized in the handler state.
--
-- The handler's **init** logic is then called as `init(effect_state, ...)`, where _effect\_state_
-- is a table shared by each instance of _kernel_'s effect. This can be used, say, to assign
-- all the data needed by some properties.
-- @param ... Initialization arguments.
function M.AddPropertyState (kernel, handler_name, ...)
	local prop_handler = assert(Handlers[handler_name], "Invalid handler")

	assert(not kernel.graph, "Cannot add property state to multi-pass kernel")	

	-- If this is the first property, configure and register the property data.
	local name = _GetName_(kernel)
	local pdata = PropertyData[name] or { handlers = {}, effect_state = {} }
	local handlers, effect_state = pdata.handlers, pdata.effect_state -- capture these rather than pdata itself
	local props = pdata.properties or {
		get = function(t, k, state)
			local v = state[k]

			if v == nil then
				for handler in pairs(handlers) do
					v = handler.get(t, k, state, effect_state)

					if v ~= nil then
						return v
					end
				end
			end
		end,

		set = function(t, k, v, state)
			for handler in pairs(handlers) do
				if handler.set(t, k, v, state, effect_state) then
					return
				end
			end

			return "none"
		end
	}

	PropertyData[name], pdata.properties = pdata, props

	-- If this is the first time this handler has been added to the kernel, register the
	-- handler and add the keys to the state. Initialize the property state.
	if not handlers[prop_handler] then
		for i = 1, #prop_handler, 2 do
			effect_state[prop_handler[i]] = prop_handler[i + 1] and {}
		end

		handlers[prop_handler] = true
	end

	prop_handler.init(pdata.effect_state, ...)
end

--- Variant of @{AddPropertyState} that takes a vertex datum argument.
--
-- The kernel may not define uniform userdata.
-- @ptable kernel Corona shader kernel.
-- @param handler_name As per @{AddPropertyState}.
-- @ptable vertex_datum Vertex userdata component, which is assumed to be associated with the
-- new state. I is added to _kernel_.**vertexData** (if absent, this is first created).
--
-- This function is merely for convenience, meant to keep the parameter definition and adding
-- of state together in the calling code.
-- @param ... Initialization arguments, as per @{AddPropertyState}.
function M.AddPropertyState_VertexDatum (kernel, handler_name, vertex_datum, ...)
	-- TODO: assert no uniform data

	local vdata = kernel.vertexData or {}

	vdata[#vdata + 1] = vertex_datum

	kernel.vertexData = vdata

	_AddPropertyState_(kernel, handler_name, ...)
end

-- Effect -> state map --
local State = setmetatable({}, { __mode = "k" })

-- Lazily gets an augmented effect instance's property accessors
local function GetAccessors (effect, name)
	local effect_mt, pdata = getmetatable(effect), PropertyData[name]
	local props = pdata and pdata.properties

	if props and not pdata.proxy then
		local get, index = props.get, effect_mt.__index
		local set, newindex = props.set, effect_mt.__newindex

		pdata.proxy, State[effect] = {
			-- Getter --
			get = get and function(t, k, object)
				local v = index(t, k)

				if v ~= nil then
					return v
				else
					return get(t, k, State[t], object)
				end
			end or index,

			-- Setter --
			set = set and function(t, k, v, object)
				if set(t, k, v, State[t], object) == "none" then
					newindex(t, k, v)
				end
			end or newindex
		}, {}
	end

	return pdata and pdata.proxy
end

-- Fill effect -> proxy map --
local Proxy = setmetatable({}, { __mode = "k" })

--- If no property data is associated with _name_'s effect, calling this is equivalent to
-- the assignment `object.fill.effect = name`.
--
-- Otherwise, the effect, after being assigned, is augmented so that @{GetEffectProperty}
-- and @{SetEffectProperty} pick up on any extended properties.
-- @pobject object Display object.
-- @string name Effect name to assign, cf. @{GetName}.
function M.AssignEffect (object, name)
	object.fill.effect = name

	-- Multi-pass: attach accessors to each component effect.
	local effect, nodes = object.fill.effect, MultiPass[name]

	if nodes then
		for k, v in pairs(nodes) do
			local sub_effect = effect[k]

			if not Proxy[sub_effect] then
				Proxy[sub_effect] = GetAccessors(sub_effect, v)
			end
		end

	-- Otherwise, attach accessors to the single effect.
	elseif not Proxy[effect] then
		Proxy[effect] = GetAccessors(effect, name)
	end
end

--- Adds a handler used to resolve properties out of effect state.
-- @param name Friendly name of the new property type.
-- @callable get Routine used to get a property value associated with this handler, cf.
-- @{GetEffectProperty}.
-- @callable set Routined used to set a property value associated with this handler, cf.
-- @{SetEffectProperty}.
-- @callable init Routine used to initialize property state associated with this handler, cf.
-- @{AddPropertyState}.
-- @callable has_prop Routine used to check whether an effect has a property associated with
-- this handler, cf. @{FoundInProperties}.
-- @array[opt] keys If present, an array of the form `{ name1, is_table1, ..., namen, is_tablen }`,
-- where each _name?_ must not have been given in any previous call to **DefinePropertyHandler**.
--
-- When this type of property state is first added to an effect's property data, the _name?_
-- keys in the effect state are populated: if the corresponding _is\_table?_ is true, the
-- value will be an empty table, otherwise **false**.
--
-- Non-table keys are meant to be changed, but well-behaved setters must leave tables intact,
-- instead assigning only to their contents. Getters and property predicates, meanwhile,
-- ought to be read-only.
function M.DefinePropertyHandler (name, get, set, init, has_prop, keys)
	assert(not Handlers[name], "Property handler name already in use")

	-- Ensure that no keys belong to an already-defined handler.
	for i = 1, #(keys or ""), 2 do
		assert(not Keys[keys[i]], "Key already in use")
	end

	-- Put the keys into a more convenient form and register all the handler functions.
	local handler = { get = get, set = set, init = init, has_prop = has_prop }

	for i = 1, #(keys or ""), 2 do
		Keys[keys[i]] = true

		handler[#handler + 1] = keys[i]
		handler[#handler + 1] = not not keys[i + 1]
	end

	Handlers[name] = handler
end

--- Predicate.
-- @string name Resolved effect name, cf. @{GetName}.
-- @string prop Property to find. This may be composite, cf. @{ParseProperty}, in which
-- case the effect is assumed to be multi-pass and a pass is used in its place.
-- @treturn boolean Does _prop_ belong to the effect?
-- @treturn ?number If the property exists, its minimum value...
-- @treturn ?number ...and maximum value.
function M.FoundInProperties (name, prop)
	return _FoundInProperties_Parsed_(name, _ParseProperty_(prop))
end

--- Variant of @{FoundInProperties} with the property parsed, as per @{ParseProperty}.
-- @string name Resolved effect name, cf. @{GetName}.
-- @string[opt] pass If present, the name of the effect pass.
-- @string prop Property to find.
-- @treturn boolean Does _prop_ belong to the effect?
-- @treturn ?number If the property exists, its minimum value...
-- @treturn ?number ...and maximum value.
function M.FoundInProperties_Parsed (name, pass, prop)
	-- If the property would be in a pass, search in that instead.
	if pass then
		name = assert(MultiPass[name], "Kernel is not multi-pass")[pass]
	end

	-- If one of the effect's property predicates, passes, report success along with the
	-- minimum and maximum property values; otherwise, fail.
	local pdata = PropertyData[name]

	if pdata then
		local effect_state = pdata.effect_state

		for handler in pairs(pdata.handlers) do
			local does_have, min, max = handler.has_prop(effect_state, prop)

			if does_have then
				return true, min, max
			end
		end
	end

	return false
end

-- Multi-pass-aware helper to resolve effect
local function GetEffect (object, pass)
	local effect = object.fill.effect

	if pass then
		return effect[pass]
	else
		return effect
	end
end

--- If found, `object.fill.effect[prop]` (or `object.fill.effect[pass][sub_prop]`) is used.
--
-- Otherwise, the effect's getters are called one by one, as `get(effect, prop, state, effect_state)`,
-- the final two arguments being tables: _effect\_state_ is shared by all instances of the
-- effect, whereas _state_ is per-instance (these are exclusively for **get** and **set** to use).
--
-- If a non-**nil** value is returned along the way, it is returned.
-- @pobject object Object with effect under **fill.effect**.
-- @string prop Property to find. This may be composite, cf. @{ParseProperty}, in which
-- case the effect is assumed to be multi-pass and a pass is used in its place.
-- @return Property value, or **nil** if absent.
function M.GetEffectProperty (object, prop)
	return _GetEffectProperty_Parsed_(object, _ParseProperty_(prop))
end

--- Variant of @{GetEffectProperty} with the property parsed, as per @{ParseProperty}.
-- @pobject object Object with effect under **fill.effect**.
-- @string[opt] pass If present, the name of the effect pass.
-- @string prop Property to get.
-- @return Property value, or **nil** if absent.
function M.GetEffectProperty_Parsed (object, pass, prop)
	local effect = GetEffect(object, pass)
	local proxy = Proxy[effect]

	if proxy then
		return proxy.get(effect, prop, object)
	else
		return effect[prop]
	end
end

--- Gets the name used to assign the effect defined by the kernel.
-- @ptable kernel Corona shader kernel.
-- @treturn string Resolved effect name.
function M.GetName (kernel)
	return format("%s.%s.%s", kernel.category, kernel.group or "custom", kernel.name)
end

--- Parses a property, which may be composite, i.e. of the form **"pass.name"**.
-- @string prop Property to parse.
-- @treturn ?string If _prop_ is composite, _pass_; otherwise, **nil**.
-- @treturn string If _prop_ is composite, _name_; otherwise, _prop_.
function M.ParseProperty (prop)
	local dot = find(prop, "%.")

	if dot then
		return sub(prop, 1, dot - 1), sub(prop, dot + 1)
	else
		return nil, prop
	end
end

--- If _prop_ is found in the effect, this is the same as `object.fill.effect[prop] = v` or
-- `object.fill.effect[pass][sub_prop] = v`.
--
-- Otherwise, the effect's setters are called one by one, as `found = set(effect, prop, v, state, effect_state)`,
-- the final two arguments being tables: _effect\_state_ is shared by all instances of the
-- effect, whereas _state_ is per-instance (these are exclusively for **get** and **set** to use).
--
-- If _found_ is true, iteration stops.
-- @pobject object Object with effect under **fill.effect**.
-- @string prop Property to find. This may be composite, cf. @{ParseProperty}, in which
-- case the effect is assumed to be multi-pass and a pass is used in its place.
-- @param v Value to assign.
function M.SetEffectProperty (object, prop, v)
	local pass, eprop = _ParseProperty_(prop)

	_SetEffectProperty_Parsed_(object, pass, eprop, v)
end

--- Variant of @{SetEffectProperty} with the property parsed, as per @{ParseProperty}.
-- @pobject object Object with effect under **fill.effect**.
-- @string[opt] pass If present, the name of the effect pass.
-- @string prop Property to set.
-- @param v Value to assign.
function M.SetEffectProperty_Parsed (object, pass, prop, v)
	local effect = GetEffect(object, pass)
	local proxy = Proxy[effect]

	if proxy then
		proxy.set(effect, prop, v, object)
	else
		effect[prop] = v
	end
end

--- Wraps a display object, which allows `v = wrapper[k]` and `wrapper[k] = v` in place of
-- `GetEffectProperty(object, k)` and `SetEffectProperty(object, k, v)`, respectively.
-- @pobject object Display object.
-- @treturn EffectWrapper Wrapper object.
-- @see GetEffectProperty, SetEffectProperty
function M.Wrap (object)
	local wrapper = {
		__index = function(_, k)
			return _GetEffectProperty_(object, k)
		end,

		__newindex = function(_, k, v)
			_SetEffectProperty_(object, k, v)
		end
	}

	return setmetatable(wrapper, wrapper)
end

-- Cache module members.
_AddPropertyState_ = M.AddPropertyState
_FoundInProperties_Parsed_ = M.FoundInProperties_Parsed
_GetEffectProperty_ = M.GetEffectProperty
_GetEffectProperty_Parsed_ = M.GetEffectProperty_Parsed
_GetName_ = M.GetName
_ParseProperty_ = M.ParseProperty
_SetEffectProperty_ = M.SetEffectProperty
_SetEffectProperty_Parsed_ = M.SetEffectProperty_Parsed

-- Export the module.
return M