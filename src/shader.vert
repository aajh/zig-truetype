#version 410 core

uniform uvec2 screen_size;

layout(location = 0) in vec2 vertex_position_screen_space;
layout(location = 1) in vec2 vertex_glyph_coordinate;
layout(location = 2) in float vertex_pixels_in_funit;
layout(location = 3) in int vertex_glyph_start;
layout(location = 4) in int vertex_glyph_end;

layout(location = 0) out vec2 glyph_coordinate;
layout(location = 1) out float pixels_in_funit;
layout(location = 2) flat out int glyph_start;
layout(location = 3) flat out int glyph_end;

void main() {
    gl_Position.xy = 2.0*(vertex_position_screen_space / vec2(screen_size)) - 1.0;
    gl_Position.z = 0.0;
    gl_Position.w = 1.0;

    glyph_coordinate = vertex_glyph_coordinate;
    pixels_in_funit = vertex_pixels_in_funit;
    glyph_start = vertex_glyph_start;
    glyph_end = vertex_glyph_end;
}

