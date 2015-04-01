--- GLSL-side mixins to acquire data sent by @{corona_shader.lua.pack}'s routines.

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

-- Export the functions.
return {

[[
	vec2 Unpack_UnitPair (float xy)
	{
		P_UV float axy = abs(xy);
		P_UV float frac = fract(axy);

		return vec2((axy - frac) * (1. / 1024.), sign(xy) * frac + .5);
	}
]], [[
	void Unpack_UnitPair4 (vec4 xy, out vec4 x, out vec4 y)
	{
		P_UV vec4 axy = abs(xy);
		P_UV vec4 frac = fract(axy);

		x = (axy - frac) * (1. / 1024.);
		y = sign(xy) * frac + .5;
	}
]]

}