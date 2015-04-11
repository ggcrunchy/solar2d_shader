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
local concat = table.concat
local gmatch = string.gmatch
local gsub = string.gsub
local ipairs = ipairs
local loaded = package.loaded
local pairs = pairs
local require = require
local sort = table.sort
local type = type

-- Modules --
local strings = require("tektite_core.var.strings")

-- Cached module references --
local _VertexShader_

-- Exports --
local M = {}

-- Build up a list of names to ignore when gathering identifiers --
local IgnoreThese = {}

for _, v in ipairs{
	"for", "if", "return", "while", -- keywords
	"__FILE__", "__LINE__", "__VERSION__", "GL_ES", "GL_FRAGMENT_PRECISION_HIGH", -- predefined macros
	"bool", "int", "float", -- singleton constructors
	"bvec2", "bvec3", "bvec4", -- vector / matrix constructors
	"ivec2", "ivec3", "ivec4",
	"mat2", "mat3", "mat4",
	"vec2", "vec3", "vec4",
	"radians", "degrees", "sin", "cos", "tan", "atan", "asin", "acos", -- angle / trig
	"pow", "exp", "log", "exp2", "log2", "sqrt", "inversesqrt", -- exponential
	"abs", "sign", "floor", "ceil", "fract", "mod", "min", "max", "clamp", "mix", "step", "smoothstep", -- common
	"length", "distance", "dot", "cross", "normalize", "faceforward", "reflect", "refract", -- geometric
	"matrixCompMult", -- matrix
	"lessThan", "lessThanEqual", "greaterThan", "greaterThanEqual", "equal", "notEqual", "any", "all", "not", -- vector relational
	"texture2D", "texture2DProj", "textureCube", "texture2DLod", "texture2DProjLod", "textureCubeLod", -- texture lookup
	"gl_Position", "gl_PointSize", -- vertex shader outputs
	"gl_FragCoord", "gl_FrontFacing", "gl_PointCoord", -- fragment shader inputs
	"gl_FragColor", "glFragData", -- fragment shader outputs
	"gl_MaxVertexAttribs", "gl_MaxVertexUniformVectors", "gl_MaxVaryingVectors", "gl_MaxVertexTextureImageUnits", -- built-in constants
	"gl_MaxCombinedTextureImageUnits", "gl_MaxTextureImageUnits", "gl_MaxFragmentUniformVectors", "gl_MaxDrawBuffers",
	"gl_DepthRangeParameters", "gl_DepthRange", -- built-in uniform state
	"none", "const", "attribute", "uniform", "varying", -- storage qualifiers
	"in", "out", "inout", -- parameter qualifiers
	"highp", "mediump", "lowp", "precision", -- precision qualifiers
	"invariant", "STDGL", -- invariant qualifiers
	"CoronaVertexUserData", -- Corona data-passing
	"CoronaSampler0", "CoronaSampler1", -- Corona samplers
	"CoronaContentScale", "CoronaDeltaTime", "CoronaTotalTime", "CoronaTexCoord", "CoronaTexelSize", -- Corona environment variables
	"CoronaColorScale", -- Corona functions
	"FragmentKernel", "VertexKernel" -- Kernel mains
} do
	IgnoreThese[v] = true
end

-- TODO: are arrays well-handled?
-- What about preprocessor stuff? (including extensions and the associated behaviors)

-- Is the name okay to gather?
local function Accepts (ignore, what)
	return not (IgnoreThese[what] or (ignore and ignore[what]))
end

-- Zero or more spaces --
local SpacesPatt = "%s*"

-- C-style comments replacement
local function CommentC (before, up_to, comment, after, last)
	local has_up_to = gsub(up_to, SpacesPatt, "") ~= ""
	local has_after = gsub(after, SpacesPatt, "") ~= ""

	if has_up_to and has_after then
		local mid = gsub(comment, "[^\n]", "") ~= "" and "\n" or " "

		return before .. up_to .. mid .. after .. last
	elseif has_up_to then
		return before .. up_to .. last
	elseif has_after then
		return before .. after .. last
	else
		return before
	end
end

-- C++-style comments replacement
local function CommentCPP (before, between, last)
	if gsub(between, SpacesPatt, "") ~= "" then
		return before .. between .. last
	else
		return before
	end
end

-- Strips comments from source (to avoid spurious identifiers)
local function EatComments (str)
	str = gsub(str, "(\n?)([^\n]-)/%*(.-)%*/([^\n]*)(\n?)", CommentC)
	str = gsub(str, "(\n?)([^\n]-)//[^\n]*(\n?)", CommentCPP)

	return str
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
		return nil, OneString, nil, true -- dummy iteration (already done)
	else
		local input = require(name)

		if type(input) == "table" then
			local ignore

			for i = 1, #(input.ignore or "") do
				ignore = ignore or {}

				ignore[input.ignore[i]] = true
			end

			return ignore, ipairs(input)
		else
			return nil, OneString, input
		end
	end
end

-- Registered code segment; identifier-to-code segment ID map; next available segment ID --
local Code, Names, ID = {}, {}, 1

-- A legal GLSL function or variable identifier --
local IdentifierPatt = "[_%a][_%w]*"

-- Optional dot --
local DotPatt = "(%.?)"

-- Assignment to a variable, struct field, or vector components --
local AssignmentPatt = DotPatt .. SpacesPatt .. "(" .. IdentifierPatt .. ")" .. SpacesPatt .. "="

-- ^^^^ TODO: Matrices, arrays okay?

-- Loads one or more constants-defining code segments
local function LoadConstants (input)
	local ignore, f, s, v = GetInputAndIgnoreList(input)

	for _, str in f, s, v do
		str = EatComments(str)

		-- Associate any unignored constants with the code segment, after disambiguating them
		-- from structure fields or vector components.
		for dot, name in gmatch(str, AssignmentPatt) do
			if dot == "" and Accepts(ignore, name) then
				Names[name] = ID
			end
		end

		-- Register the code.
		ID, Code[ID] = ID + 1, str
	end
end

-- Names depended on by code segements, i.e. coming from other segments --
local DependsOn = {}

-- Braced / parenthesized substrings; optional punctuation character --
local BracesPatt = "%b{}"
local ParensPatt = "%b()"
local PunctPatt = "(%p?)"

-- Dummy capture (shader source is zero-terminated) for pattern consistency --
local ZeroPatt = "(%z?)"

-- Function calls, definitions; struct declarations; variable usage --
local IterCallsPatt = "(" .. IdentifierPatt .. ")" .. SpacesPatt .. ParensPatt .. SpacesPatt .. PunctPatt
local IterDefsPatt = ZeroPatt .. "(" .. IdentifierPatt .. ")" .. SpacesPatt .. ParensPatt .. SpacesPatt .. BracesPatt
local IterStructsPatt = ZeroPatt .. "struct%s+(" .. IdentifierPatt .. ")"
local IterVarsPatt = DotPatt .. SpacesPatt .. "(" .. IdentifierPatt .. ")" .. SpacesPatt .. PunctPatt

-- Loads one or more functions-defining code segments
local function LoadFunctions (input)
	local ignore, f, s, v = GetInputAndIgnoreList(input)

	for _, str in f, s, v do
		str = EatComments(str)

		-- Find any variables (which must be distinguished from structure fields and vector
		-- components) and function calls (which must be distinguished from definitions). If
		-- these are not to be ignored (e.g. local functions or built-ins), add their names
		-- to the dependencies (n.b. this can harmlessly self-reference the code segment).
		local depends_on

		for dot, name, token in gmatch(str, IterVarsPatt) do
			if dot == "" and token ~= "(" and token ~= "{" and Names[name] then
				depends_on = depends_on or {}

				depends_on[name] = true
			end
		end

		for name, token in gmatch(str, IterCallsPatt) do
			if token ~= "{" and Accepts(ignore, name) then
				depends_on = depends_on or {}

				depends_on[name] = true
			end
		end

		-- Associate function and structure definitions to the code segment. For all intents
		-- and purposes, the latter (i.e. constructors) are interpreted as functions.
		for _, name in gmatch(str, IterDefsPatt) do
			if Accepts(ignore, name) then
				Names[name] = ID
			end
		end

		for _, name in gmatch(str, IterStructsPatt) do
			if Accepts(ignore, name) then
				Names[name] = ID
			end
		end

		-- Register the code and its dependencies.
		ID, Code[ID], DependsOn[ID] = ID + 1, str, depends_on
	end
end

-- Visits a node during topological search
local function Visit (list, marks, index)
	if not marks[index] then
		marks[index] = true

		local deps = DependsOn[index]

		if deps then
			for name in pairs(deps) do
				Visit(list, marks, Names[name])
			end
		end

		list[#list + 1] = index
	end
end

--- DOCME
-- single string, array of strings, ignores
-- @ptable params Load parameters. Fields:
--
-- * **from**:
-- * **prefix**:
-- * **constants**:
-- * **functions**:
function M.Load (params)
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

	-- Register any new constants or functions.
	for i = 1, #(params.constants or "") do
		LoadConstants(prefix .. params.constants[i])
	end

	for i = 1, #(params.functions or "") do
		LoadFunctions(prefix .. params.functions[i])
	end

	-- TODO: Detect conflicts (before committing anything to be sorted!)

	-- Topologically sort the registered code segments.
	local list, marks = {}, {}

	for i = 1, ID do
		Visit(list, marks, i)
	end
end

-- Gather the ID's of a segment and its dependencies
local function CollectDependencies (collect, id)
	local deps = DependsOn[id]

	if deps then
		for name in pairs(deps) do
			local dep_id = Names[name]

			if dep_id ~= id then
				CollectDependencies(collect, dep_id)
			end
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

-- Gather names to be ignored
local function BuildIgnoreList (code, patt, ignore)
	for dot, name in gmatch(code, patt) do
		if dot == "" and Accepts(ignore, name) then
			ignore = ignore or {}

			ignore[name] = true
		end
	end

	return ignore
end

-- Infers depended-on code to prepend
local function Include (code)
	-- Ignore any local assignments and definitions.
	local ignore

	ignore = BuildIgnoreList(code, AssignmentPatt, ignore)
	ignore = BuildIgnoreList(code, IterDefsPatt, ignore)
	ignore = BuildIgnoreList(code, IterStructsPatt, ignore)

	-- Collect all external dependencies, with any necessary disambiguation (less care is
	-- needed here since much has already been registered).
	local collect

	for dot, name in gmatch(code, IterVarsPatt) do
		if dot == "" and Accepts(ignore, name) then 
			collect = CollectName(collect, name)
		end
	end

	for name in gmatch(code, IterCallsPatt) do
		if Accepts(ignore, name) then
			collect = CollectName(collect, name)
		end
	end

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
-- @string code Shader-specific code.
-- @ptable[opt] opts Shader options. Fields:
--
-- * **pretty**: If true, the code will be somewhat prettied for printing.
-- * **no_default_precision**: If true, no default precision qualifier is established for
-- this shader. **N.B.** This will not play well with most of the registered helper code.
-- @treturn string Resolved shader code.
function M.FragmentShader (code, opts)
	code = _VertexShader_(code, opts)

	if not (opts and opts.no_default_precision) then
		code = Pretty(Prelude, opts) .. code
	end

	return code
end

--- As per @{FragmentShader}, but for vertex shaders.
-- @string code Shader-specific code.
-- @ptable[opt] opts Shader options. Fields:
--
-- * **pretty**: If true, the code will be somewhat prettied for printing.
-- @treturn string Resolved shader code.
function M.VertexShader (code, opts)
	code = EatComments(code)

	local include = Include(code)

	if include then
		code = include .. "\n" .. code
	end

	return Pretty(code, opts)
end

-- Cache module members.
_VertexShader_ = M.VertexShader

-- Export the module.
return M