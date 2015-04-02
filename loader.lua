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
local lower = string.lower
local pairs = pairs
local require = require
local sort = table.sort
local sub = string.sub
local type = type
local upper = string.upper

-- Exports --
local M = {}

-- --
local IgnoreThese = {}

for _, v in ipairs{
	"for", "if", "return", "while", -- keywords
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
	"CoronaVertexUserData", -- Corona data-passing
	"CoronaSampler0", "CoronaSampler1", -- Corona samplers
	"CoronaContentScale", "CoronaDeltaTime", "CoronaTotalTime", "CoronaTexCoord", "CoronaTexelSize", -- Corona environment variables
	"CoronaColorScale", -- Corona functions
	"FragmentKernel", "VertexKernel" -- Kernel mains
} do
	IgnoreThese[v] = true
end
-- TODO: struct constructors need own handling... :/

--
local function Accepts (ignore, what)
	return not (IgnoreThese[what] or (ignore and ignore[what]))
end

--
local function SlashComment (str)
	return sub(str, -1) == "\n" and "\n" or ""
end

--
local function EatComments (str)
	str = gsub(str, "/%*.*%*/", "")
	str = gsub(str, "//[^\n]*\n?", SlashComment)

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
	local input, ignore = require("corona_shader.glsl." .. name)

	if type(input) == "table" then
		return input.ignore, ipairs(input)
	else
		return nil, OneString, input
	end
end

-- --
local Code, Names, ID = {}, {}, 1

-- --
local IdentifierPatt = "[_%a][_%w]*"

-- --
local SpacesPatt = "%s*"

-- --
local AssignmentPatt = "(" .. IdentifierPatt .. ")" .. SpacesPatt .. "="

--
local function LoadConstants (input)
	local ignore, f, s, v = GetInputAndIgnoreList(input)

	for _, str in f, s, v do
		str = EatComments(str)

		for name in gmatch(str, AssignmentPatt) do
			if Accepts(ignore, name) then
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

-- --
local IterCallsPatt = "(" .. IdentifierPatt .. ")" .. SpacesPatt .. ParensPatt .. SpacesPatt .. PunctPatt
local IterDefsPatt = "(" .. IdentifierPatt .. ")" .. SpacesPatt .. ParensPatt .. SpacesPatt .. BracesPatt
local IterVarsPatt = "(" .. IdentifierPatt .. ")" .. SpacesPatt .. PunctPatt

--
local function LoadFunctions (input)
	local ignore, f, s, v = GetInputAndIgnoreList(input)

	--
	for _, str in f, s, v do
		local depends_on

		str = EatComments(str)

		for name, token in gmatch(str, IterVarsPatt) do
			if token ~= "(" and token ~= "{" and Names[name] then
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
		for name in gmatch(str, IterDefsPatt) do
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
local function Include (code)
	local collect

	--
	for name in gmatch(code, IterVarsPatt) do
		collect = CollectName(collect, name)
	end

	--
	for name, token in gmatch(code, IterCallsPatt) do
		if token ~= "{" and Accepts(nil, name) then
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

-- --
local Prelude = [[
	#ifdef GL_ES
		precision mediump float;
	#endif

]]

--- DOCME
function M.FragmentShader (code, suppress_precision)
	code = EatComments(code)

	local include = Include(code)

	if include then
		code = include .. "\n" .. code
	end

	if not suppress_precision then
		code = Prelude .. code
	end

	return code
end

--- DOCME
function M.VertexShader (code)
	local include = Include(code)

	if include then
		code = include .. "\n" .. code
	end

	return code
end

-- Export the module.
return M