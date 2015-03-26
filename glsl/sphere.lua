--- Sphere routines.

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

-- Modules --
local constants = require("corona_shader.glsl.constants")

-- Exports --
local M = {}

--- DOCME
function M.AddSphereLogic ()
	return constants.AddPi() .. [[
		vec2 GetUV (vec2 diff)
		{
			P_DEFAULT float dist_sq = dot(diff, diff);

			if (dist_sq > 1.) discard;

			P_DEFAULT float z = sqrt(1. - dist_sq);

			return vec2(.5 + atan(z, diff.x) / TWO_PI, .5 + asin(diff.y) / PI);
		}

		vec4 GetUV_ZPhi (vec2 diff)
		{
			P_DEFAULT float dist_sq = dot(diff, diff);

			if (dist_sq > 1.) discard;

			P_DEFAULT float z = sqrt(1. - dist_sq);
			P_DEFAULT float phi = atan(z, diff.x);

			return vec4(.5 + phi / TWO_PI, .5 + asin(diff.y) / PI, z, phi);
		}

		vec2 GetUV_PhiDelta (vec2 diff, float dphi)
		{
			P_DEFAULT float dist_sq = dot(diff, diff);

			if (dist_sq > 1.) discard;

			P_DEFAULT float z = sqrt(1. - dist_sq);
			P_DEFAULT float phi = atan(z, diff.x);
			P_DEFAULT float angle = .5 + phi / TWO_PI;

			angle = mod(angle + dphi, 1.);

			return vec2(angle, .5 + asin(diff.y) / PI);
		}

		vec4 GetUV_PhiDelta_ZPhi (vec2 diff, float dphi)
		{
			P_DEFAULT float dist_sq = dot(diff, diff);

			if (dist_sq > 1.) discard;

			P_DEFAULT float z = sqrt(1. - dist_sq);
			P_DEFAULT float phi = atan(z, diff.x);
			P_DEFAULT float angle = .5 + phi / TWO_PI;

			angle = mod(angle + dphi, 1.);

			return vec4(angle, .5 + asin(diff.y) / PI, z, phi);
		}

		vec3 GetTangent (vec2 diff, float phi)
		{
			// In unit sphere, diff.y = sin(theta), sqrt(1 - sin(theta)^2) = cos(theta).
			return normalize(vec3(diff.y * sin(phi + PI_OVER_TWO), diff.y * cos(phi + PI_OVER_TWO), sqrt(1. - diff.y * diff.y)));
		}
	]]
end

-- Export the module.
return M