#version 410 core

uniform uvec2 screen_size;

layout(location = 0) in vec2 vertex_position_screen_space;
layout(location = 1) in vec2 vertex_glyph_coordinate;
layout(location = 2) in int vertex_glyph_start;
layout(location = 3) in int vertex_glyph_end;

layout(location = 0) out vec2 glyph_coordinate;
layout(location = 1) flat out int glyph_start;
layout(location = 2) flat out int glyph_end;

void main() {
    gl_Position.xy = 2.0*(vertex_position_screen_space / vec2(screen_size)) - 1.0;
    gl_Position.z = 0.0;
    gl_Position.w = 1.0;

    glyph_coordinate = vertex_glyph_coordinate;
    glyph_start = vertex_glyph_start;
    glyph_end = vertex_glyph_end;
}

