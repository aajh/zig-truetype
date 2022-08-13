#version 410 core

layout(location = 0) in vec2 glyph_coordinate;

out vec3 color;

void main() {
    //color = vec3(gl_FragCoord.x / 1600, gl_FragCoord.y / 1200, 0.0);
    color = vec3((glyph_coordinate.x - 94.0)/(568.0 - 94.0), glyph_coordinate.y/708.0, 0.0);
}

