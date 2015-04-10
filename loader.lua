--- A texture-mapped sphere shader with (internally generated) bump mapping.

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
local pairs = pairs
local require = require
local sort = table.sort
local type = type

-- Cached module references --
local _VertexShader_

-- Exports --
local M = {}

-- --
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
	"gl_MaxVertexAttribs", "gl_MaxVertexUniformVectors", "gl_MaxVaryingVectors", "gl_MaxVertexTextureImageUnits", -- built-in constants...
	"gl_MaxCombinedTextureImageUnits", "gl_MaxTextureImageUnits", "gl_MaxFragmentUniformVectors", "gl_MaxDrawBuffers", -- ...continued
	"gl_DepthRangeParameters", "gl_DepthRange", -- built-in uniform state
	"none", "const", "attribute", "uniform", "varying", -- storage qualifiers
	"in", "out", "inout", -- parameter qualifiers
	"highp", "mediump", "lowp", -- precision qualifiers
	"invariant", "STDGL", -- invariant qualifiers
	"CoronaVertexUserData", -- Corona data-passing
	"CoronaSampler0", "CoronaSampler1", -- Corona samplers
	"CoronaContentScale", "CoronaDeltaTime", "CoronaTotalTime", "CoronaTexCoord", "CoronaTexelSize", -- Corona environment variables
	"CoronaColorScale", -- Corona functions
	"FragmentKernel", "VertexKernel" -- Kernel mains
} do
	IgnoreThese[v] = true
end

--
local function Accepts (ignore, what)
	return not (IgnoreThese[what] or (ignore and ignore[what]))
end

-- --
local SpacesPatt = "%s*"

--
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

--
local function CommentCPP (before, between, last)
	if gsub(between, SpacesPatt, "") ~= "" then
		return before .. between .. last
	else
		return before
	end
end

--
local function EatComments (str)
	str = gsub(str, "(\n?)([^\n]-)/%*(.-)%*/([^\n]*)(\n?)", CommentC)
	str = gsub(str, "(\n?)([^\n]-)//[^\n]*(\n?)", CommentCPP)

	return str
end

--
local function OneString (str, done)
	if not done then
		return true, str
	end
end

--
local function GetInputAndIgnoreList (name)
	local input = require("corona_shader.glsl." .. name)

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

-- --
local Code, Names, ID = {}, {}, 1

-- --
local IdentifierPatt = "[_%a][_%w]*"

-- --
local DotPatt = "(%.?)"

-- --
local AssignmentPatt = DotPatt .. SpacesPatt .. "(" .. IdentifierPatt .. ")" .. SpacesPatt .. "="

--
local function LoadConstants (input)
	local ignore, f, s, v = GetInputAndIgnoreList(input)

	for _, str in f, s, v do
		str = EatComments(str)

		for dot, name in gmatch(str, AssignmentPatt) do
			if dot == "" and Accepts(ignore, name) then
				Names[name] = ID
			end
		end

		--
		ID, Code[ID] = ID + 1, str
	end
end

--
LoadConstants("constants")

-- --
local DependsOn = {}

-- --
local BracesPatt = "%b{}"
local ParensPatt = "%b()"
local PunctPatt = "(%p?)"

-- Dummy capture (shader source is zero-terminated) for pattern consistency --
local ZeroPatt = "(%z?)"

-- --
local IterCallsPatt = "(" .. IdentifierPatt .. ")" .. SpacesPatt .. ParensPatt .. SpacesPatt .. PunctPatt
local IterDefsPatt = ZeroPatt .. "(" .. IdentifierPatt .. ")" .. SpacesPatt .. ParensPatt .. SpacesPatt .. BracesPatt
local IterStructsPatt = ZeroPatt .. "struct%s+(" .. IdentifierPatt .. ")"
local IterVarsPatt = DotPatt .. SpacesPatt .. "(" .. IdentifierPatt .. ")" .. SpacesPatt .. PunctPatt

--
local function LoadFunctions (input)
	local ignore, f, s, v = GetInputAndIgnoreList(input)

	--
	for _, str in f, s, v do
		local depends_on

		str = EatComments(str)

		for dot, name, token in gmatch(str, IterVarsPatt) do
			if dot == "" and token ~= "(" and token ~= "{" and Names[name] then
				depends_on = depends_on or {}

				depends_on[name] = true
			end
		end

		--
		for name, token in gmatch(str, IterCallsPatt) do
			if token ~= "{" and Accepts(ignore, name) then
				depends_on = depends_on or {}

				depends_on[name] = true
			end
		end

		--
		for _, name in gmatch(str, IterDefsPatt) do
			if Accepts(ignore, name) then
				Names[name] = ID
			end
		end

		--
		for _, name in gmatch(str, IterStructsPatt) do
			if Accepts(ignore, name) then
				Names[name] = ID
			end
		end

		--
		ID, Code[ID], DependsOn[ID] = ID + 1, str, depends_on
	end
end

--
LoadFunctions("bump")
LoadFunctions("neighbors")
LoadFunctions("simplex")
LoadFunctions("sphere")
LoadFunctions("texels")
LoadFunctions("unpack")
LoadFunctions("worley")

-- --
local List, Marks = {}, {}

--
local function Visit (index)
	if not Marks[index] then
		Marks[index] = true

		local deps = DependsOn[index]

		if deps then
			for name in pairs(deps) do
				Visit(Names[name])
			end
		end

		List[#List + 1] = index
	end
end

for i = 1, ID do
	Visit(i)
end

--
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

--
local function CollectName (collect, name)
	local id = Names[name]

	if id then
		collect = collect or {}

		CollectDependencies(collect, id)
	end

	return collect
end

--
local function BuildIgnoreList (code, patt, ignore)
	for dot, name in gmatch(code, patt) do
		if dot == "" and Accepts(ignore, name) then
			ignore = ignore or {}

			ignore[name] = true
		end
	end

	return ignore
end

--
local function Include (code)
	--
	local ignore

	ignore = BuildIgnoreList(code, AssignmentPatt, ignore)
	ignore = BuildIgnoreList(code, IterDefsPatt, ignore)
	ignore = BuildIgnoreList(code, IterStructsPatt, ignore)

	--
	local collect

	for dot, name in gmatch(code, IterVarsPatt) do
		if dot == "" and Accepts(ignore, name) then 
			collect = CollectName(collect, name)
		end
	end

	for name, token in gmatch(code, IterCallsPatt) do
		if token ~= "{" and Accepts(ignore, name) then
			collect = CollectName(collect, name)
		end
	end

	--
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

--
local function Pretty (str)
	return gsub(str, "\t", "  ")
end

-- --
local Prelude = Pretty[[
	#ifdef GL_ES
		#ifdef GL_FRAGMENT_PRECISION_HIGH
			precision highp float;
		#else
			precision mediump float;
		#endif
	#endif

]]

--- DOCME
function M.FragmentShader (code, opts)
	code = _VertexShader_(code, opts)

	if not (opts and opts.suppress_precision) then
		code = Prelude .. code
	end

	return code
end

--- DOCME
function M.VertexShader (code, opts)
	code = EatComments(code)

	local include = Include(code)

	if include then
		code = include .. "\n" .. code
	end

	if opts and opts.pretty then
		code = Pretty(code)
	end

	return code
end

-- Cache module members.
_VertexShader_ = M.VertexShader

-- Export the module.
return M