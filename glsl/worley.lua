--- Worley noise mixins.

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
	ignore = { "permute" },

	-- Cellular noise ("Worley noise") in 2D in GLSL.
	-- Copyright (c) Stefan Gustavson 2011-04-19. All rights reserved.
	-- This code is released under the conditions of the MIT license.
	-- See LICENSE (above).

	[[
		// Permutation polynomial: (34x^2 + x) mod 289
		vec3 permute(vec3 x) {
		  return mod((34.0 * x + 1.0) * x, 289.0);
		}

		// Cellular noise, returning F1 and F2 in a vec2.
		// Standard 3x3 search window for good F1 and F2 values
		vec2 Worley2 (vec2 P)
		{
			#define K 0.142857142857 // 1/7
			#define Ko 0.428571428571 // 3/7
			#define jitter 1.0 // Less gives more regular pattern

			vec2 Pi = mod(floor(P), 289.0);
			vec2 Pf = fract(P);
			vec3 oi = vec3(-1.0, 0.0, 1.0);
			vec3 of = vec3(-0.5, 0.5, 1.5);
			vec3 px = permute(Pi.x + oi);
			vec3 p = permute(px.x + Pi.y + oi); // p11, p12, p13
			vec3 ox = fract(p*K) - Ko;
			vec3 oy = mod(floor(p*K),7.0)*K - Ko;
			vec3 dx = Pf.x + 0.5 + jitter*ox;
			vec3 dy = Pf.y - of + jitter*oy;
			vec3 d1 = dx * dx + dy * dy; // d11, d12 and d13, squared
			p = permute(px.y + Pi.y + oi); // p21, p22, p23
			ox = fract(p*K) - Ko;
			oy = mod(floor(p*K),7.0)*K - Ko;
			dx = Pf.x - 0.5 + jitter*ox;
			dy = Pf.y - of + jitter*oy;
			vec3 d2 = dx * dx + dy * dy; // d21, d22 and d23, squared
			p = permute(px.z + Pi.y + oi); // p31, p32, p33
			ox = fract(p*K) - Ko;
			oy = mod(floor(p*K),7.0)*K - Ko;
			dx = Pf.x - 1.5 + jitter*ox;
			dy = Pf.y - of + jitter*oy;
			vec3 d3 = dx * dx + dy * dy; // d31, d32 and d33, squared

			// Sort out the two smallest distances (F1, F2)
			vec3 d1a = min(d1, d2);
			d2 = max(d1, d2); // Swap to keep candidates for F2
			d2 = min(d2, d3); // neither F1 nor F2 are now in d3
			d1 = min(d1a, d2); // F1 is now in d1
			d2 = max(d1a, d2); // Swap to keep candidates for F2
			d1.xy = (d1.x < d1.y) ? d1.xy : d1.yx; // Swap if smaller
			d1.xz = (d1.x < d1.z) ? d1.xz : d1.zx; // F1 is in d1.x
			d1.yz = min(d1.yz, d2.yz); // F2 is now not in d2.yz
			d1.y = min(d1.y, d1.z); // nor in  d1.z
			d1.y = min(d1.y, d2.x); // F2 is in d1.y, we're done.

			return sqrt(d1.xy);
		}
	]], [[
		// Permutation polynomial: (34x^2 + x) mod 289
		vec4 permute(vec4 x) {
		  return mod((34.0 * x + 1.0) * x, 289.0);
		}

		// Cellular noise, returning F1 and F2 in a vec2.
		// Speeded up by using 2x2 search window instead of 3x3,
		// at the expense of some strong pattern artifacts.
		// F2 is often wrong and has sharp discontinuities.
		// If you need a smooth F2, use the slower 3x3 version.
		// F1 is sometimes wrong, too, but OK for most purposes.
		vec2 Worley2x2(vec2 P)
		{
			#define K 0.142857142857 // 1/7
			#define K2 0.0714285714285 // K/2
			#define jitter 0.8 // jitter 1.0 makes F1 wrong more often

			vec2 Pi = mod(floor(P), 289.0);
			vec2 Pf = fract(P);
			vec4 Pfx = Pf.x + vec4(-0.5, -1.5, -0.5, -1.5);
			vec4 Pfy = Pf.y + vec4(-0.5, -0.5, -1.5, -1.5);
			vec4 p = permute(Pi.x + vec4(0.0, 1.0, 0.0, 1.0));
			p = permute(p + Pi.y + vec4(0.0, 0.0, 1.0, 1.0));
			vec4 ox = mod(p, 7.0)*K+K2;
			vec4 oy = mod(floor(p*K),7.0)*K+K2;
			vec4 dx = Pfx + jitter*ox;
			vec4 dy = Pfy + jitter*oy;
			vec4 d = dx * dx + dy * dy; // d11, d12, d21 and d22, squared

			// Sort out the two smallest distances
		#if 0
			// Cheat and pick only F1
			d.xy = min(d.xy, d.zw);
			d.x = min(d.x, d.y);

			return d.xx; // F1 duplicated, F2 not computed
		#else
			// Do it right and find both F1 and F2
			d.xy = (d.x < d.y) ? d.xy : d.yx; // Swap if smaller
			d.xz = (d.x < d.z) ? d.xz : d.zx;
			d.xw = (d.x < d.w) ? d.xw : d.wx;
			d.y = min(d.y, d.z);
			d.y = min(d.y, d.w);

			return sqrt(d.xy);
		#endif
		}
	]]
}