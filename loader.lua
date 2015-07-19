--- This module extends vertex and fragment shader loaders to infer and automatically
-- incorporate dependencies.

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
local concat = table.concat
local gmatch = string.gmatch
local gsub = string.gsub
local ipairs = ipairs
local loaded = package.loaded
local match = string.match
local pairs = pairs
local require = require
local sort = table.sort
local type = type
local wrap = coroutine.wrap
local yield = coroutine.yield

-- Modules --
local ignore_list = require("corona_shader.impl.ignore_list")
local parser = require("corona_shader.impl.parser")
local patterns = require("corona_shader.impl.patterns")
local strings = require("tektite_core.var.strings")
local table_funcs = require("tektite_core.table.funcs")

-- Cached module references --
local _VertexShader_

-- Exports --
local M = {}

-- TODO: are arrays well-handled?

-- Identifier -> code segment ID map; next available segment ID --
local Names, ID = {}, 1

-- Is the name okay to gather?
local function Accepts (ignore, what)
	return Names[what] ~= ID and ignore_list.IsAccepted(ignore, what)
end

-- Helper to add a name to a (possibly empty) list
local function AddToList (list, name)
	list = list or {}

	list[name] = true

	return list
end

-- Helper to "iterate" over a single string
local function OneString (str, done)
	if not done then
		return true, str
	end
end

-- Iterates over the input strings and builds an ignore list, if requested
local function GetInputAndIgnoreList (name)
	if loaded[name] then
		return nil, nil, OneString, nil, true -- dummy iteration (already done)
	else
		local input = require(name)

		if type(input) == "table" then
			local ignore

			for i = 1, #(input.ignore or "") do
				ignore = AddToList(ignore, input.ignore[i])
			end

			return ignore, input.replacements, ipairs(input)
		else
			return nil, nil, OneString, input
		end
	end
end

-- Registered code segment --
local Code = {}

-- ^^^^ TODO: Matrices, arrays okay?

-- Names depended on by code segements, i.e. coming from other segments --
local DependsOn = {}

-- Add names, unless ignored
local function AddNames (code, patt, ignore)
	for dot, name in gmatch(code, patt) do
		if dot == "" and Accepts(ignore, name) then
			Names[name] = ID
		end
	end
end

-- Gather depended-on names
local function BuildDependsOnList (code, patt, ignore, local_ignore, depends_on)
	for dot, name, token in gmatch(code, patt) do
		if dot == "" and token == "" and Accepts(ignore, name) and Accepts(local_ignore, name) then
			depends_on = AddToList(depends_on, name)
		end
	end

	return depends_on
end

-- Ignore identifiers used by the preprocessor
local function IgnorePreprocessor (str, ignore)
	for dir, rest in gmatch(str, patterns.Preprocessor) do
		-- #ifdef and #ifndef look at one symbol, whereas #define will have rather arbitrary
		-- contents. However, only the symbol being defined is relevant; the rest may consist
		-- of identifiers that are not to be ignored.
		if dir == "define" or dir == "ifdef" or dir == "ifndef" then
			ignore = AddToList(ignore, match(rest, patterns.Identifier))

		-- Any identifiers in #if directives, meanwhile, are symbols being compared.
		elseif dir == "if" then
			for name in gmatch(rest, patterns.Identifier) do
				ignore = AddToList(ignore, name)
			end
		end

		ignore = AddToList(ignore, dir)
	end

	return ignore
end

-- Loads one or more code segments
local function LoadSegments (input)
	local ignore, replacements, f, s, v = GetInputAndIgnoreList(input)

	for _, str in f, s, v do
		str = parser.StripComments(str)
		str = parser.InsertReplacements(str, replacements)

		-- Associate assignments to variables, as well as function and structure definitions,
		-- to the code segment. For all intents and purposes, the latter (i.e. constructors)
		-- are interpreted as functions. The contents of functions and structs are excised in
		-- order to reduce all the irrelevant assignments inside function bodies.
		local outer = gsub(str, patterns.InBraces, "{}")

		AddNames(outer, patterns.Assignment, ignore)
		AddNames(outer, patterns.DefineFunc, ignore)
		AddNames(outer, patterns.Struct, ignore)

		-- Build up dependencies, ignoring local variables. This must be done for each (non-
		-- ignored) function, since an identifier may be local to one function but refer to
		-- an actual external dependency in another.
		local depends_on

		for func, params, body in gmatch(str, patterns.Signature) do
			if not (ignore and ignore[func]) then
				-- Ignore the function's parameters, plus preprocessor directives both in the
				-- function and in the surrounding scope.
				local local_ignore

				for param in gmatch(params, patterns.Param) do
					local_ignore = AddToList(local_ignore, param)
				end

				local_ignore = IgnorePreprocessor(outer, local_ignore)
				local_ignore = IgnorePreprocessor(body, local_ignore)

				-- With the preprocessor directives and parameters addressed, filter them
				-- out of the code. Ignore any variables declared in the function body.
				local stripped = body

				stripped = gsub(stripped, patterns.Preprocessor, "")
				stripped = gsub(stripped, patterns.InParens, "()")

				for line in gmatch(stripped, patterns.Statement) do
					local vtype, var = match(line, patterns.Declaration)

					if vtype and vtype ~= "return" then
						local_ignore = AddToList(local_ignore, var)

						for extra_var in gmatch(line, patterns.Var) do
							local_ignore = AddToList(local_ignore, extra_var)
						end
					end
				end

				-- Find any variables (which must be distinguished from structure fields and vector
				-- components) and function calls (which must be distinguished from definitions). If
				-- these are not to be ignored (e.g. local functions or built-ins), add their names
				-- to the dependencies (n.b. this can harmlessly self-reference the code segment).
				depends_on = BuildDependsOnList(body, patterns.UseVar, ignore, local_ignore, depends_on)
				depends_on = BuildDependsOnList(body, patterns.Call, ignore, local_ignore, depends_on)
			end
		end

		-- Register the code and its dependencies.
		ID, Code[ID], DependsOn[ID] = ID + 1, str, depends_on
	end
end

-- Visits a node during topological search
local function Visit (list, marks, index, from)
	local mark = marks[index]

	-- Not yet visited: proceed.
	if mark == nil then
		assert(index, index or ("Unable to resolve symbol: " .. from))

		marks[index] = false

		local deps = DependsOn[index]

		if deps then
			for name in pairs(deps) do
				Visit(list, marks, Names[name], name)
			end
		end

		list[#list + 1], marks[index] = index, true

	-- Being visited: cycle.
	elseif mark == false then
		yield(from, "cycle")
	end
end

--- Loads code segments for later use by @{FragmentShader} and @{VertexShader}.
--
-- The various code components (constants, functions, structs, variables) are gathered and
-- the associated segments are ordered topologically, to ensure well-formed shaders.
--
-- Each module being submitted is expected to return either a string or a table.
--
-- As a string, this is a single code segment.
--
-- As a table, the array part will consist of strings, each being a code segment. An array
-- of strings may also be found under the **ignore** key; any identifiers listed here are
-- ignored when examining the code segments, e.g. local functions or intermediate constants.
--
-- Already-loaded modules are ignored.
-- single string, array of strings, ignores
-- @ptable params Load parameters. Fields:
--
-- * **from**: If present, the name of the module calling **Load**, whose directory will be
-- used as per **prefix**.
-- * **prefix**: If present (and **from** is absent), prefixed to the name of each module to
-- be loaded.
--
-- Names of code segment modules are placed in the array part.
--
-- Assignments in the outermost scope, e.g. `vec2 var = vec2(1.0, 3.5)`, in addition to
-- definitions for functions, e.g. `void action (out vec2 v) { ... }` and structs, e.g.
-- `struct data { ... }`, are examined. Any identifiers, such as _var_, _action_, and _data_
-- in these cases, if not found in the ignore list, are registered with the loader.
-- @treturn boolean Loading succeeded?
-- @treturn ?string If loading failed, an error message.
function M.Load (params)
	-- Make a snapshot of the state, in case loading goes awry.
	local id, code, depends_on, names = ID, Code, DependsOn, Names

	Code = table_funcs.Copy(Code)
	DependsOn = table_funcs.Copy(DependsOn)
	Names = table_funcs.Copy(Names)

	-- Prepare any prefix for module names.
	local prefix = ""

	if params.from then
		prefix = strings.RemoveLastSubstring(params.from, "%.")
	elseif params.prefix then
		prefix = params.prefix
	end

	if #prefix > 0 then
		prefix = prefix .. "."
	end

	-- Register any new code segments.
	for _, v in ipairs(params) do
		LoadSegments(prefix .. v)
	end

	-- Topologically sort the registered code segments. If a cycle was introduced, revert any
	-- changes and report the (first) troublesome identifier.
	local list, marks = {}, {}

	for i = 1, ID do
		local name = wrap(Visit)(list, marks, i)

		if name then
			ID, Code, DependsOn, Names = id, code, depends_on, names

			return false, "Cycle found with identifier: " .. name
		end
	end

	return true
end

-- Gather names to be ignored
local function BuildIgnoreList (code, patt, ignore)
	for dot, name in gmatch(code, patt) do
		if dot == "" and Accepts(ignore, name) then
			ignore = AddToList(ignore, name)
		end
	end

	return ignore
end

-- Gather the ID's of a segment and its dependencies
local function CollectDependencies (collect, id)
	local deps = DependsOn[id]

	if deps then
		for name in pairs(deps) do
			CollectDependencies(collect, Names[name])
		end
	end

	collect[#collect + 1] = id
end

-- Gather the dependencies for a variable or function
local function CollectName (collect, name)
	local id = Names[name]

	if id then
		collect = collect or {}

		CollectDependencies(collect, id)
	end

	return collect
end

-- Gathers names to collect
local function CollectIntoList (code, patt, ignore, collect)
	for dot, name in gmatch(code, patt) do
		if dot == "" and Accepts(ignore, name) then 
			collect = CollectName(collect, name)
		end
	end

	return collect
end

-- Infers depended-on code to prepend
local function Include (code)
	-- Ignore any local assignments and definitions.
	local ignore

	ignore = BuildIgnoreList(code, patterns.Assignment, ignore)
	ignore = BuildIgnoreList(code, patterns.DefineFunc, ignore)
	ignore = BuildIgnoreList(code, patterns.Struct, ignore)
	ignore = IgnorePreprocessor(code, ignore)

	-- Collect all external dependencies, with any necessary disambiguation (less care is
	-- needed here since much has already been registered).
	local collect

	collect = CollectIntoList(code, patterns.UseVar, ignore, collect)
	collect = CollectIntoList(code, patterns.Call, ignore, collect)

	-- If any dependencies were found, put them into topologically-sorted order, remove any
	-- duplicates, stitch them together, and return the result.
	if collect then
		sort(collect)

		local pieces, prev = {}

		for i = 1, #collect do
			local id = collect[i]

			if id ~= prev then
				pieces[#pieces + 1] = Code[id]
			end

			prev = id
		end

		return concat(pieces, "\n")
	end
end

-- Helper to make source reasonably print-friendly
local function Pretty (str, opts)
	if opts and opts.pretty then
		return gsub(str, "\t", "  ")
	else
		return str
	end
end

-- Common fragment shader prelude --
local Prelude = [[
	#define FRAGMENT_SHADER

	#ifdef GL_ES
		#ifdef GL_FRAGMENT_PRECISION_HIGH
			precision highp float;
		#else
			precision mediump float;
		#endif
	#endif

]]

--- Given some fragment shader code, this will attempt to resolve any dependencies on
-- registered GLSL helpers, in particular various constants, functions, structs, and
-- variables.
--
-- If there are errors in the shader source, e.g. unknown names, this function will still
-- return its best effort, say to print out for debugging. Since the code will probably be
-- broken, an error will show up when calling **graphics.defineEffect** down the line.
-- @tparam ?|string|table opts Shader options.
--
-- If this is a string, the shader-specific code.
--
-- Otherwise, a table which may contain these fields:
--
-- * **main**: Shader-specific code. This field is required.
-- * **prelude**: If present, this string is prepended (as is) to the final shader, e.g. to
-- perform **#define**'s.
-- * **pretty**: If true, the code will be somewhat prettied for printing.
-- * **no_default_precision**: If true, no default precision qualifier is established for
-- this shader. **N.B.** This will not play well with most of the registered helper code.
-- @treturn string Resolved shader code.
function M.FragmentShader (opts)
	local code = _VertexShader_(opts)

	if not (type(opts) == "table" and opts.no_default_precision) then
		code = Pretty(Prelude, opts) .. code
	end

	return code
end

--- As per @{FragmentShader}, but for vertex shaders.
-- @tparam ?|string|table opts Shader options.
--
-- If this is a string, the shader-specific code.
--
-- Otherwise, a table which may contain these fields:
--
-- * **main**: Shader-specific code. This field is required.
-- * **prelude**: If present, this string is prepended (as is) to the final shader, e.g. to
-- perform **#define**'s.
-- * **pretty**: If true, the code will be somewhat prettied for printing.
-- @treturn string Resolved shader code.
function M.VertexShader (opts)
	local is_table, code = type(opts) == "table"

	if is_table then
		code = assert(opts.main, "Missing shader-specific code")
	else
		code = opts
	end

	code = parser.StripComments(code)

	local include = Include(code)

	if include then
		code = include .. "\n" .. code
	end

	if is_table and opts.prelude then
		code = opts.prelude .. "\n" .. code
	end

	return Pretty(code, opts)
end

-- Cache module members.
_VertexShader_ = M.VertexShader

-- Export the module.
return M