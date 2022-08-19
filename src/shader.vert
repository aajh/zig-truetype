#version 410 core

uniform uvec2 screen_size;

layout(location = 0) in vec2 vertex_position_screen_space;
layout(location = 1) in vec2 vertex_glyph_coordinate;

layout(location = 0) out vec2 glyph_coordinate;

void main() {
    gl_Position.xy = 2.0*(vertex_position_screen_space / vec2(screen_size)) - 1.0;
    gl_Position.z = 0.0;
    gl_Position.w = 1.0;

    glyph_coordinate = vertex_glyph_coordinate;
}

