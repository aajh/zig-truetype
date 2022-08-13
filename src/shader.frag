#version 410 core

uniform isamplerBuffer curves;
uniform int glyph_start;
uniform int glyph_len;

layout(location = 0) in vec2 glyph_coordinate;

out vec3 color;

float dot2(in vec2 v) { return dot(v,v); }
float cro(in vec2 a, in vec2 b) { return a.x*b.y - a.y*b.x; }

// From https://iquilezles.org/articles/distfunctions2d/
float sdSegment(in vec2 p, in vec2 a, in vec2 b) {
    vec2 pa = p-a, ba = b-a;
    float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
    return length( pa - ba*h );
}

// From https://www.shadertoy.com/view/MlKcDD
// Adapted from http://research.microsoft.com/en-us/um/people/hoppe/ravg.pdf (https://hhoppe.com/ravg.pdf)
float sdBezier(vec2 p, vec2 v0, vec2 v1, vec2 v2) {
    if (v0 == v1) return sdSegment(p, v0, v2);

    vec2 i = v0 - v2;
    vec2 j = v2 - v1;
    vec2 k = v1 - v0;
    vec2 w = j-k;

    v0-= p; v1-= p; v2-= p;

    float x = cro(v0, v2);
    float y = cro(v1, v0);
    float z = cro(v2, v1);

    vec2 s = 2.0*(y*j+z*k)-x*i;

    float r =  (y*z-x*x*0.25)/dot2(s);
    float t = clamp( (0.5*x+y+r*dot(s,w))/(x+y+z),0.0,1.0);

    return length( v0+t*(k+k+t*w) );
}

float sample_y(float t, vec2 p0, vec2 p1, vec2 p2) {
    return t*t * (p0.y - 2*p1.y + p2.y) + t * (2*p1.y - 2*p0.y) + p0.y;
}

// From https://git.outerproduct.net/dconf2021-ff.git/
int windingDelta(vec2 loc, vec2 p0, vec2 p1, vec2 p2) {
    vec2 res;
    bvec2 valid;

    float a = p0.x - 2*p1.x + p2.x;
    float b = 2*p1.x - 2*p0.x;
    float c = p0.x - loc.x;
    float det = b*b - 4*a*c;
    res = vec2((-b - sqrt(det)) / (2 * a),
            (-b + sqrt(det)) / (2 * a));
    valid.x = valid.y = det >= 0;
    if (abs(a) < 1e-3) {
        res.x = res.y = -c / b;
        valid.x = valid.y = true;
    }


    //todo why must I repeat myself?
    valid.x = valid.x && (sample_y(res.x, p0, p1, p2) < loc.y);
    valid.y = valid.y && (sample_y(res.y, p0, p1, p2) < loc.y);

    int ret = 0;
    int klass = 0x2e74 >> ((p0.x > loc.x ? 2 : 0) + (p1.x > loc.x ? 4 : 0) + (p2.x > loc.x ? 8 : 0));
    if ((klass & 1) != 0 && valid.x) {
        ret++;
    }
    if ((klass & 2) != 0 && valid.y) {
        ret--;
    }

    return ret;
}

vec2 getCurvePoint(int curve_i, int point_i) {
    int coordinate = 3*curve_i + point_i;
    return vec2(texelFetch(curves, coordinate));
}

float sdGlyph(vec2 p) {
    float distance = 1.0 / 0.0;
    int winding = 0;
    for (int i = glyph_start; i < glyph_start + glyph_len; ++i) {
        vec2 p0 = getCurvePoint(i, 0);
        vec2 p1 = getCurvePoint(i, 1);
        vec2 p2 = getCurvePoint(i, 2);
        float d = sdBezier(p, p0, p1, p2);
        if (abs(d) < abs(distance)) {
            distance = d;
        }
        winding += windingDelta(p, p0, p1, p2);
    }
    return winding != 0 ? -distance : distance;
}

void main() {
    float signed_distance = sdGlyph(glyph_coordinate);
    float c = 1.0 - clamp(0.5 - signed_distance, 0, 1);
    color = vec3(c, c, c);

    //color = vec3(gl_FragCoord.x / 1600, gl_FragCoord.y / 1200, 0.0);
    //color = vec3((glyph_coordinate.x - 94.0)/(568.0 - 94.0), glyph_coordinate.y/708.0, 0.0);
}

