--- Utilities for ignoring things in the loader.

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
local ipairs = ipairs

-- Exports --
local M = {}

--
--
--

-- Build up a list of names to ignore when gathering identifiers.
local IgnoreThese = {}

for _, v in ipairs{
	"break", "continue", "discard", "do", "else", "for", "if", "return", "while", "true", "false", -- keywords
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
	"P_COLOR", "P_DEFAULT", "P_POSITION", "P_RANDOM", "P_UV", -- Corona qualifiers
	"CoronaVertexUserData", -- Corona data-passing
	"CoronaSampler0", "CoronaSampler1", -- Corona samplers
	"CoronaContentScale", "CoronaDeltaTime", "CoronaTotalTime", "CoronaTexCoord", "CoronaTexelSize", -- Corona environment variables
	"CoronaColorScale", -- Corona functions
	"FragmentKernel", "VertexKernel" -- Kernel mains
} do
	IgnoreThese[v] = true
end

--- Indicates whether a name is accepted after checking against a master list, as well as any
-- user-provided ignore list. 
-- @ptable[opt] ignore_list If present, _name_ is ignored when the value at its key is true.
-- @string name Name to check.
-- @treturn boolean Name is accepted?
function M.IsAccepted (ignore_list, name)
	if IgnoreThese[name] then
		return false
	else
		return not (ignore_list and ignore_list[name])
	end
end

return M