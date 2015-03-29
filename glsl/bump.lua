--- Bump mapping mixins.

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

-- Exports --
local M = {}

--- DOCME
function M.AddBumpMapGenerators ()
	return [[
		vec3 ComputeNormal (sampler2D s, vec2 uv, vec3 tcolor)
		{
			P_COLOR vec3 right = texture2D(s, uv + vec2(CoronaTexelSize.x, 0.)).rgb;
			P_COLOR vec3 above = texture2D(s, uv + vec2(0., CoronaTexelSize.y)).rgb;
			P_COLOR float rz = dot(right - tcolor, vec3(1.));
			P_COLOR float uz = dot(above - tcolor, vec3(1.));

			return normalize(vec3(-uz, -rz, 1.));
		}

		vec3 ComputeNormal (sampler2D s, vec2 uv)
		{
			return ComputeNormal(s, uv, texture2D(s, uv).rgb);
		}

		vec3 EncodeNormal (sampler2D s, vec2 uv, vec3 tcolor)
		{
			return .5 * (ComputeNormal(s, uv, tcolor) + 1.);
		}

		vec3 EncodeNormal (sampler2D s, vec2 uv)
		{
			return .5 * (ComputeNormal(s, uv) + 1.);
		}
	]]
end

--- DOCME
function M.AddBumpMapLogic ()
	return [[
		vec3 GetWorldNormal_TS (vec3 bump, vec3 T, vec3 B, vec3 N)
		{
			return T * bump.x + B * bump.y + N * bump.z;
		}

		vec3 GetWorldNormal (vec3 bump, vec3 T, vec3 N)
		{
			return GetWorldNormal_TS(bump, T, cross(N, T), N);
		}

		vec3 GetWorldNormal (sampler2D s, vec2 uv, vec3 tcolor, vec3 T, vec3 N)
		{
			return GetWorldNormal(ComputeNormal(s, uv, tcolor), T, N);
		}

		vec3 GetWorldNormal (sampler2D s, vec2 uv, vec3 T, vec3 N)
		{
			return GetWorldNormal(ComputeNormal(s, uv), T, N);
		}

		vec3 GetWorldNormal_TS (sampler2D s, vec2 uv, vec3 tcolor, vec3 T, vec3 B, vec3 N)
		{
			return GetWorldNormal_TS(ComputeNormal(s, uv, tcolor), T, B, N);
		}

		vec3 GetWorldNormal_TS (sampler2D s, vec2 uv, vec3 T, vec3 B, vec3 N)
		{
			return GetWorldNormal_TS(ComputeNormal(s, uv), T, B, N);
		}
	]]
end

-- Export the module.
return M