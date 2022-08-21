#version 410 core

uniform uvec2 screen_size;

layout(location = 0) in vec2 vertex_position_screen_space;
layout(location = 1) in vec2 vertex_glyph_coordinate;
layout(location = 2) in vec2 vertex_funits_per_pixel;
layout(location = 3) in int vertex_glyph_start;
layout(location = 4) in int vertex_glyph_end;

layout(location = 0) out vec2 glyph_coordinate;
layout(location = 1) flat out int glyph_start;
layout(location = 2) flat out int glyph_end;

void main() {
    vec2 pixel_expansion;
    switch(gl_VertexID % 4) {
        case 0:
            pixel_expansion = vec2(-1, -1);
            break;
        case 1:
            pixel_expansion = vec2( 1, -1);
            break;
        case 2:
            pixel_expansion = vec2(-1,  1);
            break;
        case 3:
            pixel_expansion = vec2( 1,  1);
            break;
    }

    gl_Position.xy = 2.0*((vertex_position_screen_space + pixel_expansion) / vec2(screen_size)) - 1.0;
    gl_Position.z = 0.0;
    gl_Position.w = 1.0;

    glyph_coordinate = vertex_glyph_coordinate + vertex_funits_per_pixel*pixel_expansion;
    glyph_start = vertex_glyph_start;
    glyph_end = vertex_glyph_end;
}

