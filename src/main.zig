const std = @import("std");
const builtin = @import("builtin");
const gl = @import("gl");
const c = @import("c.zig");
const truetype = @import("truetype.zig");

const Font = truetype.Font;
const QuadraticBezierCurve = truetype.QuadraticBezierCurve;

const Allocator = std.mem.Allocator;
const Vector = std.meta.Vector;

fn logSdlError(message: []const u8, src: std.builtin.SourceLocation) void {
    std.debug.print("{s} error {s}:{}: {s}\n", .{ message, src.file, src.line, c.SDL_GetError() });
}

fn getHighDpiFactorGL(window: *c.SDL_Window) f32 {
    var window_width: i32 = undefined;
    var window_height: i32 = undefined;
    c.SDL_GetWindowSize(window, &window_width, &window_height);

    var drawable_width: i32 = undefined;
    var drawable_height: i32 = undefined;
    c.SDL_GL_GetDrawableSize(window, &drawable_width, &drawable_height);

    return @intToFloat(f32, drawable_width) / @intToFloat(f32, window_width);
}

const GlyphAtlas = struct {
    const AtlasGlyph = struct {
        x_min: i16,
        y_min: i16,
        x_max: i16,
        y_max: i16,

        start: i32,
        end  : i32,
    };

    gpa: Allocator,
    font: *Font,
    glyphs: std.AutoHashMap(u32, ?AtlasGlyph),
    curves: std.ArrayList(QuadraticBezierCurve),
    new_curves: bool = false,

    fn init(gpa: Allocator, font: *Font) GlyphAtlas {
        return .{
            .gpa = gpa,
            .font = font,
            .glyphs = std.AutoHashMap(u32, ?AtlasGlyph).init(gpa),
            .curves = std.ArrayList(QuadraticBezierCurve).init(gpa),
        };
    }

    fn deinit(self: *GlyphAtlas) void {
        self.glyphs.deinit();
        self.curves.deinit();
    }

    fn getGlyph(self: *GlyphAtlas, glyph_index: u32) !?AtlasGlyph {
        return self.glyphs.get(glyph_index) orelse {
            const font_glyph = (try self.font.getGlyph(glyph_index)) orelse {
                try self.glyphs.put(glyph_index, null);
                return null;
            };
            defer font_glyph.deinit();

            const start = self.curves.items.len;
            try self.curves.appendSlice(font_glyph.segments);
            const end = self.curves.items.len;

            const atlas_glyph = AtlasGlyph{
                .x_min = font_glyph.x_min,
                .y_min = font_glyph.y_min,
                .x_max = font_glyph.x_max,
                .y_max = font_glyph.y_max,
                .start = @intCast(i32, start),
                .end   = @intCast(i32, end),
            };
            try self.glyphs.put(glyph_index, atlas_glyph);
            self.new_curves = true;
            return atlas_glyph;
        };
    }

    fn uploadCurves(self: *GlyphAtlas, texture_buffer: gl.GLuint, texture: gl.GLuint) void {
        if (!self.new_curves) return;

        gl.bindBuffer(gl.TEXTURE_BUFFER, texture_buffer);
        // TODO: Upload only new curves.
        gl.bufferData(gl.TEXTURE_BUFFER, @intCast(gl.GLsizeiptr, @sizeOf(QuadraticBezierCurve)*self.curves.items.len), self.curves.items.ptr, gl.STATIC_DRAW);

        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_BUFFER, texture);
        gl.texBuffer(gl.TEXTURE_BUFFER, gl.RG16I, texture_buffer);

        self.new_curves = false;
    }
};

const GraphicsContext = struct {
    window    : *c.SDL_Window,
    gl_context: c.SDL_GLContext = null,

    vertex_array: gl.GLuint = 0,
    shader      : gl.GLuint = 0,

    vertex_index_buffer           : gl.GLuint = 0,
    vertex_position_buffer        : gl.GLuint = 0,
    vertex_glyph_coordinate_buffer: gl.GLuint = 0,
    vertex_funits_per_pixel_buffer: gl.GLuint = 0,
    vertex_glyph_start_buffer     : gl.GLuint = 0,
    vertex_glyph_end_buffer       : gl.GLuint = 0,
    curves_buffer                 : gl.GLuint = 0,

    curves_texture: gl.GLuint = 0,

    screen_size_location      : gl.GLint = 0,
    view_matrix_location      : gl.GLint = 0,
    projection_matrix_location: gl.GLint = 0,
    curves_location           : gl.GLint = 0,

    vertex_indices          : std.ArrayList(gl.GLushort),
    vertex_positions        : std.ArrayList(f32),
    vertex_glyph_coordinates: std.ArrayList(f32),
    vertex_funits_per_pixels: std.ArrayList(f32),
    vertex_glyph_starts     : std.ArrayList(gl.GLint),
    vertex_glyph_ends       : std.ArrayList(gl.GLint),

    atlas                   : GlyphAtlas,
    font_blob               : ?*c.hb_blob_t,
    hb_face                 : ?*c.hb_face_t,
    hb_font                 : ?*c.hb_font_t,

    fn init(gpa: Allocator, window: *c.SDL_Window, font: *Font) !GraphicsContext {
        const font_blob = c.hb_blob_create(font.font_file_content.ptr, @intCast(c_uint, font.font_file_content.len), c.HB_MEMORY_MODE_READONLY, null, null);
        errdefer c.hb_blob_destroy(font_blob);
        const hb_face = c.hb_face_create(font_blob, 0);
        errdefer c.hb_face_destroy(hb_face);
        const hb_font = c.hb_font_create(hb_face);
        errdefer c.hb_font_destroy(hb_font);

        var gc: GraphicsContext = .{
            .window = window,
            .vertex_indices           = std.ArrayList(gl.GLushort).init(gpa),
            .vertex_positions         = std.ArrayList(f32).init(gpa),
            .vertex_glyph_coordinates = std.ArrayList(f32).init(gpa),
            .vertex_funits_per_pixels = std.ArrayList(f32).init(gpa),
            .vertex_glyph_starts      = std.ArrayList(gl.GLint).init(gpa),
            .vertex_glyph_ends        = std.ArrayList(gl.GLint).init(gpa),
            .atlas                    = GlyphAtlas.init(gpa, font),
            .font_blob                = font_blob,
            .hb_face                  = hb_face,
            .hb_font                  = hb_font,
        };
        errdefer {
            gc.vertex_indices.deinit();
            gc.vertex_positions.deinit();
            gc.vertex_glyph_coordinates.deinit();
            gc.vertex_funits_per_pixels.deinit();
            gc.vertex_glyph_starts.deinit();
            gc.vertex_glyph_ends.deinit();
            gc.atlas.deinit();
        }

        gc.gl_context = c.SDL_GL_CreateContext(window) orelse {
            logSdlError("SDL_GL_CreateContext", @src());
            return error.SDLGLCreateContextFailed;
        };
        errdefer c.SDL_GL_DeleteContext(gc.gl_context);

        var major_version: c_int = undefined;
        if (c.SDL_GL_GetAttribute(c.SDL_GL_CONTEXT_MAJOR_VERSION, &major_version) != 0) {
            logSdlError("SDL_GL_GetAttribute", @src());
            return error.SDLGLSetAttributeFailed;
        }
        var minor_version: c_int = undefined;
        if (c.SDL_GL_GetAttribute(c.SDL_GL_CONTEXT_MINOR_VERSION, &minor_version) != 0) {
            logSdlError("SDL_GL_GetAttribute", @src());
            return error.SDLGLSetAttributeFailed;
        }
        std.debug.print("OpenGL version {d}.{d}\n", .{ major_version, minor_version });

        try gl.load(window, glGetProcAddress);

        gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
        gl.enable(gl.BLEND);

        gl.genVertexArrays(1, &gc.vertex_array);
        errdefer gl.deleteVertexArrays(1, &gc.vertex_array);
        gl.bindVertexArray(gc.vertex_array);

        gc.shader = shader: {
            const vertex_shader = gl.createShader(gl.VERTEX_SHADER);
            const fragment_shader = gl.createShader(gl.FRAGMENT_SHADER);

            const vertex_shader_code = @embedFile("shader.vert");
            const fragment_shader_code = @embedFile("shader.frag");

            var result: gl.GLint = gl.FALSE;
            var info_log_length: gl.GLint = 0;

            gl.shaderSource(vertex_shader, 1, &[_][*c]const u8{ vertex_shader_code }, null);
            gl.compileShader(vertex_shader);

            gl.getShaderiv(vertex_shader, gl.COMPILE_STATUS, &result);
            gl.getShaderiv(vertex_shader, gl.INFO_LOG_LENGTH, &info_log_length);
            if (info_log_length > 0) {
                var info_log = try gpa.alloc(u8, @intCast(usize, info_log_length));
                defer gpa.free(info_log);
                gl.getShaderInfoLog(vertex_shader, info_log_length, null, info_log.ptr);
                std.debug.print("{s}\n", .{ info_log });
            }
            if (result == gl.FALSE) {
                return error.VertexShaderCompilationFailed;
            }

            gl.shaderSource(fragment_shader, 1, &[_][*c]const u8{ fragment_shader_code }, null);
            gl.compileShader(fragment_shader);

            gl.getShaderiv(fragment_shader, gl.COMPILE_STATUS, &result);
            gl.getShaderiv(fragment_shader, gl.INFO_LOG_LENGTH, &info_log_length);
            if (info_log_length > 0) {
                var info_log = try gpa.alloc(u8, @intCast(usize, info_log_length));
                defer gpa.free(info_log);
                gl.getShaderInfoLog(fragment_shader, info_log_length, null, info_log.ptr);
                std.debug.print("{s}\n", .{ info_log });
            }
            if (result == gl.FALSE) {
                return error.FragmentShaderCompilationFailed;
            }

            const program = gl.createProgram();
            gl.attachShader(program, vertex_shader);
            gl.attachShader(program, fragment_shader);
            gl.linkProgram(program);

            gl.getProgramiv(program, gl.LINK_STATUS, &result);
            gl.getProgramiv(program, gl.INFO_LOG_LENGTH, &info_log_length);
            if (info_log_length > 0) {
                var info_log = try gpa.alloc(u8, @intCast(usize, info_log_length));
                defer gpa.free(info_log);
                gl.getProgramInfoLog(program, info_log_length, null, info_log.ptr);
                std.debug.print("{s}\n", .{ info_log });
            }
            if (result == gl.FALSE) {
                return error.ShaderLinkingFailed;
            }

            gl.detachShader(program, vertex_shader);
            gl.detachShader(program, fragment_shader);

            gl.deleteShader(vertex_shader);
            gl.deleteShader(fragment_shader);

            break :shader program;
        };
        errdefer gl.deleteProgram(gc.shader);
        gl.useProgram(gc.shader);

        gl.genBuffers(1, &gc.vertex_index_buffer);
        gl.genBuffers(1, &gc.vertex_position_buffer);
        gl.genBuffers(1, &gc.vertex_glyph_coordinate_buffer);
        gl.genBuffers(1, &gc.vertex_funits_per_pixel_buffer);
        gl.genBuffers(1, &gc.vertex_glyph_start_buffer);
        gl.genBuffers(1, &gc.vertex_glyph_end_buffer);
        gl.genBuffers(1, &gc.curves_buffer);
        errdefer {
            const buffers = [_]gl.GLuint {
                gc.vertex_index_buffer,
                gc.vertex_position_buffer,
                gc.vertex_glyph_coordinate_buffer,
                gc.vertex_funits_per_pixel_buffer,
                gc.vertex_glyph_start_buffer,
                gc.vertex_glyph_end_buffer,
                gc.curves_buffer,
            };
            gl.deleteBuffers(buffers.len, &buffers[0]);
        }

        gl.genTextures(1, &gc.curves_texture);
        errdefer gl.deleteTextures(1, &gc.curves_texture);

        gc.screen_size_location       = gl.getUniformLocation(gc.shader, "screen_size");
        gc.view_matrix_location       = gl.getUniformLocation(gc.shader, "view_matrix");
        gc.projection_matrix_location = gl.getUniformLocation(gc.shader, "projection_matrix");
        gc.curves_location            = gl.getUniformLocation(gc.shader, "curves");

        return gc;
    }

    fn deinit(gc: *GraphicsContext) void {
        gl.deleteTextures(1, &gc.curves_texture);
        var buffers = [_]gl.GLuint {
            gc.vertex_index_buffer,
            gc.vertex_position_buffer,
            gc.vertex_glyph_coordinate_buffer,
            gc.vertex_funits_per_pixel_buffer,
            gc.vertex_glyph_start_buffer,
            gc.vertex_glyph_end_buffer,
            gc.curves_buffer,
        };
        gl.deleteBuffers(buffers.len, &buffers[0]);
        gl.deleteProgram(gc.shader);
        gl.deleteVertexArrays(1, &gc.vertex_array);
        c.SDL_GL_DeleteContext(gc.gl_context);

        gc.vertex_indices.deinit();
        gc.vertex_positions.deinit();
        gc.vertex_glyph_coordinates.deinit();
        gc.vertex_funits_per_pixels.deinit();
        gc.vertex_glyph_starts.deinit();
        gc.vertex_glyph_ends.deinit();
        gc.atlas.deinit();

        c.hb_font_destroy(gc.hb_font);
        c.hb_face_destroy(gc.hb_face);
        c.hb_blob_destroy(gc.font_blob);
    }

    fn shapeText(gc: GraphicsContext, text: []const u8) !*c.hb_buffer_t {
        const hb_buffer = c.hb_buffer_create() orelse {
            return error.HbBufferCreateFailed;
        };
        c.hb_buffer_add_utf8(hb_buffer, &text[0], @intCast(c_int, text.len), 0, @intCast(c_int, text.len));

        c.hb_buffer_set_direction(hb_buffer, c.HB_DIRECTION_LTR);
        c.hb_buffer_set_script(hb_buffer, c.HB_SCRIPT_LATIN);
        c.hb_buffer_set_language(hb_buffer, c.hb_language_from_string("en", -1));

        c.hb_shape(gc.hb_font, hb_buffer, null, 0);

        return hb_buffer;
    }

    fn getFontAscenderSize(gc: GraphicsContext, pixels_per_em: f32) f32 {
        const pixels_per_funit = pixels_per_em / @intToFloat(f32, gc.atlas.font.head_table.units_per_em);
        return @intToFloat(f32, gc.atlas.font.hhea_table.ascent) * pixels_per_funit;
    }

    fn getFontDescenderSize(gc: GraphicsContext, pixels_per_em: f32) f32 {
        const pixels_per_funit = pixels_per_em / @intToFloat(f32, gc.atlas.font.head_table.units_per_em);
        return @intToFloat(f32, gc.atlas.font.hhea_table.descent) * pixels_per_funit;
    }

    fn getTextSize(gc: GraphicsContext, shaped_text: ?*c.hb_buffer_t, pixels_per_em: f32, width: ?*f32, height: ?*f32) void {
        const pixels_per_funit = pixels_per_em / @intToFloat(f32, gc.atlas.font.head_table.units_per_em);
        if (width != null and shaped_text != null) {
            var glyph_count: u32 = undefined;
            var glyph_pos = c.hb_buffer_get_glyph_positions(shaped_text, &glyph_count);

            var w: f32 = 0;
            var index: usize = 0;
            while (index < glyph_count) : (index += 1) {
                const x_advance = @intToFloat(f32, glyph_pos[index].x_advance) * pixels_per_funit;
                w += x_advance;
            }
            width.?.* = w;
        }
        if (height != null) {
            const ascender = @intToFloat(f32, gc.atlas.font.hhea_table.ascent) * pixels_per_funit;
            const descender = @intToFloat(f32, gc.atlas.font.hhea_table.descent) * pixels_per_funit;
            const h = ascender - descender;
            height.?.* = h;
        }
    }

    fn drawGlyph(gc: *GraphicsContext, glyph_index: u32, x0: f32, y0: f32, pixels_per_em: f32) !void {
        const glyph = (try gc.atlas.getGlyph(glyph_index)) orelse {
            // The glyph_index has no glyph to render, i.e. it is whitespace or similar.
            return;
        };

        const i = @intCast(gl.GLushort, gc.vertex_indices.items.len / 6 * 4);
        const vertex_indices_data = [_]gl.GLushort {
            i    , i + 1, i + 2,
            i + 2, i + 1, i + 3,
        };
        try gc.vertex_indices.appendSlice(&vertex_indices_data);

        // pixels_per_funit
        const ppf = pixels_per_em / @intToFloat(f32, gc.atlas.font.head_table.units_per_em);

        const x = x0 + @intToFloat(f32, glyph.x_min)*ppf;
        const y = y0 + @intToFloat(f32, glyph.y_min)*ppf;
        const w = @intToFloat(f32, glyph.x_max - glyph.x_min)*ppf;
        const h = @intToFloat(f32, glyph.y_max - glyph.y_min)*ppf;

        const vertex_position_data = [_]f32{
            x, y,
            x + w, y,
            x, y + h,
            x + w, y + h,
        };
        try gc.vertex_positions.appendSlice(&vertex_position_data);

        const vertex_glyph_coordinate_data = [_]f32 {
            @intToFloat(f32, glyph.x_min), @intToFloat(f32, glyph.y_min),
            @intToFloat(f32, glyph.x_max), @intToFloat(f32, glyph.y_min),
            @intToFloat(f32, glyph.x_min), @intToFloat(f32, glyph.y_max),
            @intToFloat(f32, glyph.x_max), @intToFloat(f32, glyph.y_max),
        };
        try gc.vertex_glyph_coordinates.appendSlice(&vertex_glyph_coordinate_data);

        try gc.vertex_funits_per_pixels.appendNTimes(1 / ppf, 8);

        try gc.vertex_glyph_starts.appendNTimes(glyph.start, 4);
        try gc.vertex_glyph_ends.appendNTimes(glyph.end, 4);
    }

    fn drawText(gc: *GraphicsContext, shaped_text: *c.hb_buffer_t, x: f32, y: f32, pixels_per_em: f32) !void {
        var glyph_count: u32 = undefined;
        var glyph_info = c.hb_buffer_get_glyph_infos(shaped_text, &glyph_count);
        var glyph_pos = c.hb_buffer_get_glyph_positions(shaped_text, &glyph_count);

        const pixels_per_funit = pixels_per_em / @intToFloat(f32, gc.atlas.font.head_table.units_per_em);

        var cursor_x: f32 = x;
        var cursor_y: f32 = y;
        var index: usize = 0;
        while (index < glyph_count) : (index += 1) {
            const glyph_index = glyph_info[index].codepoint;
            const x_offset = @intToFloat(f32, glyph_pos[index].x_offset) * pixels_per_funit;
            const y_offset = @intToFloat(f32, glyph_pos[index].y_offset) * pixels_per_funit;
            const x_advance = @intToFloat(f32, glyph_pos[index].x_advance) * pixels_per_funit;
            const y_advance = @intToFloat(f32, glyph_pos[index].y_advance) * pixels_per_funit;

            try gc.drawGlyph(glyph_index, cursor_x + x_offset, cursor_y - y_offset, pixels_per_em);

            cursor_x += x_advance;
            cursor_y += y_advance;
        }

    }

    fn flush(gc: *GraphicsContext, use_perspective: bool) void {
        gc.atlas.uploadCurves(gc.curves_buffer, gc.curves_texture);

        gl.useProgram(gc.shader);

        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, gc.vertex_index_buffer);
        gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, @intCast(gl.GLsizeiptr, @sizeOf(gl.GLushort)*gc.vertex_indices.items.len), gc.vertex_indices.items.ptr, gl.DYNAMIC_DRAW);

        gl.enableVertexAttribArray(0);
        gl.bindBuffer(gl.ARRAY_BUFFER, gc.vertex_position_buffer);
        gl.bufferData(gl.ARRAY_BUFFER, @intCast(gl.GLsizeiptr, @sizeOf(f32)*gc.vertex_positions.items.len), gc.vertex_positions.items.ptr, gl.DYNAMIC_DRAW);
        gl.vertexAttribPointer(0, 2, gl.FLOAT, gl.FALSE, 0, null);

        gl.enableVertexAttribArray(1);
        gl.bindBuffer(gl.ARRAY_BUFFER, gc.vertex_glyph_coordinate_buffer);
        gl.bufferData(gl.ARRAY_BUFFER, @intCast(gl.GLsizeiptr, @sizeOf(f32)*gc.vertex_glyph_coordinates.items.len), gc.vertex_glyph_coordinates.items.ptr, gl.DYNAMIC_DRAW);
        gl.vertexAttribPointer(1, 2, gl.FLOAT, gl.FALSE, 0, null);

        gl.enableVertexAttribArray(2);
        gl.bindBuffer(gl.ARRAY_BUFFER, gc.vertex_funits_per_pixel_buffer);
        gl.bufferData(gl.ARRAY_BUFFER, @intCast(gl.GLsizeiptr, @sizeOf(f32)*gc.vertex_funits_per_pixels.items.len), gc.vertex_funits_per_pixels.items.ptr, gl.DYNAMIC_DRAW);
        gl.vertexAttribPointer(2, 2, gl.FLOAT, gl.FALSE, 0, null);

        gl.enableVertexAttribArray(3);
        gl.bindBuffer(gl.ARRAY_BUFFER, gc.vertex_glyph_start_buffer);
        gl.bufferData(gl.ARRAY_BUFFER, @intCast(gl.GLsizeiptr, @sizeOf(gl.GLint)*gc.vertex_glyph_starts.items.len), gc.vertex_glyph_starts.items.ptr, gl.DYNAMIC_DRAW);
        gl.vertexAttribIPointer(3, 1, gl.INT, 0, null);

        gl.enableVertexAttribArray(4);
        gl.bindBuffer(gl.ARRAY_BUFFER, gc.vertex_glyph_end_buffer);
        gl.bufferData(gl.ARRAY_BUFFER, @intCast(gl.GLsizeiptr, @sizeOf(gl.GLint)*gc.vertex_glyph_ends.items.len), gc.vertex_glyph_ends.items.ptr, gl.DYNAMIC_DRAW);
        gl.vertexAttribIPointer(4, 1, gl.INT, 0, null);

        var drawable_width: i32 = undefined;
        var drawable_height: i32 = undefined;
        c.SDL_GL_GetDrawableSize(gc.window, &drawable_width, &drawable_height);
        gl.uniform2ui(gc.screen_size_location, @intCast(gl.GLuint, drawable_width), @intCast(gl.GLuint, drawable_height));

        if (use_perspective) {
            {
                const eye = Vector(3, f32){ -1.2, -0.3, 0.5 };
                const center = Vector(3, f32){ 0, 0, 0 };
                const up = Vector(3, f32){ 1, 1, 0 };

                var Z = eye - center;
                Z = Z / @splat(3, std.math.sqrt(Z[0]*Z[0] + Z[1]*Z[1] + Z[2]*Z[2]));
                var Y = up;
                var X = Vector(3, f32){ Y[1]*Z[2] - Y[2]*Z[1], Y[2]*Z[0] - Y[0]*Z[2], Y[0]*Z[1] - Y[1]*Z[0] };
                Y = Vector(3, f32){ Z[1]*X[2] - Z[2]*X[1], Z[2]*X[0] - Z[0]*X[2], Z[0]*X[1] - Z[1]*X[0] };
                X = X / @splat(3, std.math.sqrt(X[0]*X[0] + X[1]*X[1] + X[2]*X[2]));
                Y = Y / @splat(3, std.math.sqrt(Y[0]*Y[0] + Y[1]*Y[1] + Y[2]*Y[2]));

                const view_matrix_data = [_]f32{
                    X[0], X[1], X[2], -(X[0]*eye[0] + X[1]*eye[1] + X[2]*eye[2]),
                    Y[0], Y[1], Y[2], -(Y[0]*eye[0] + Y[1]*eye[1] + Y[2]*eye[2]),
                    Z[0], Z[1], Z[2], -(Z[0]*eye[0] + Z[1]*eye[1] + Z[2]*eye[2]),
                    0, 0, 0, 1,
                };
                gl.uniformMatrix4fv(gc.view_matrix_location, 1, gl.TRUE, &view_matrix_data[0]);
            }
            {
                const angle_of_view = 90;
                const image_aspect_ratio = @intToFloat(f32, drawable_width) / @intToFloat(f32, drawable_height);
                const n = 0.1;
                const f  = 100;

                const scale = std.math.tan(@as(f32, angle_of_view * 0.5 * std.math.pi / 180.0)) * n;
                const r = image_aspect_ratio * scale;
                const l = -r;
                const t = scale;
                const b = -t;

                const projection_matrix_data = [_]f32{
                    2 * n / (r - l), 0, (r + l) / (r - l), 0,
                    0, 2 * n / (t - b), (t + b) / (t - b), 0,
                    0, 0, -(f + n) / (f - n), -2 * f * n / (f - n),
                    0, 0, -1, 0,
                };
                gl.uniformMatrix4fv(gc.projection_matrix_location, 1, gl.TRUE, &projection_matrix_data[0]);
            }
        } else {
            const identity_matrix = [_]f32{
                1, 0, 0, 0,
                0, 1, 0, 0,
                0, 0, 1, 0,
                0, 0, 0, 1,
            };
            gl.uniformMatrix4fv(gc.view_matrix_location, 1, gl.FALSE, &identity_matrix[0]);
            gl.uniformMatrix4fv(gc.projection_matrix_location, 1, gl.FALSE, &identity_matrix[0]);
        }

        gl.uniform1i(gc.curves_location, 0);

        gl.activeTexture(gl.TEXTURE0);

        gl.drawElements(gl.TRIANGLES, @intCast(gl.GLsizei, gc.vertex_indices.items.len), gl.UNSIGNED_SHORT, null);

        gl.disableVertexAttribArray(0);
        gl.disableVertexAttribArray(1);
        gl.disableVertexAttribArray(2);
        gl.disableVertexAttribArray(3);
        gl.disableVertexAttribArray(4);

        gc.vertex_indices.clearRetainingCapacity();
        gc.vertex_positions.clearRetainingCapacity();
        gc.vertex_glyph_coordinates.clearRetainingCapacity();
        gc.vertex_funits_per_pixels.clearRetainingCapacity();
        gc.vertex_glyph_starts.clearRetainingCapacity();
        gc.vertex_glyph_ends.clearRetainingCapacity();
    }
};

fn glGetProcAddress(window: *c.SDL_Window, proc: [:0]const u8) ?*const anyopaque {
    _ = window;
    return c.SDL_GL_GetProcAddress(proc);
}

pub fn main() !void {
    var gpa_instance = std.heap.GeneralPurposeAllocator(.{}){};
    var gpa = gpa_instance.allocator();

    var font = font: {
        var directory = std.fs.cwd();
        var font_file = try directory.openFile("AvenirNext-Regular-08.ttf", .{ .read = true });
        defer font_file.close();

        break :font try Font.initWithFile(gpa, font_file);
    };
    defer font.deinit();

    std.debug.print("\n", .{});

    if (c.SDL_SetHint(c.SDL_HINT_WINDOWS_DPI_AWARENESS, "system") != c.SDL_TRUE) {
        logSdlError("SDL_SetHint", @src());
        return error.SDLSetHintFailed;
    }
    if (c.SDL_SetHint(c.SDL_HINT_WINDOWS_DPI_SCALING , "1") != c.SDL_TRUE) {
        logSdlError("SDL_SetHint", @src());
        return error.SDLSetHintFailed;
    }

    if (c.SDL_Init(c.SDL_INIT_VIDEO) != 0) {
        logSdlError("SDL_Init", @src());
        return error.SDLInitFailed;
    }
    defer c.SDL_Quit();

    if (c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_PROFILE_MASK, c.SDL_GL_CONTEXT_PROFILE_CORE) != 0) {
        logSdlError("SDL_GL_SetAttribute", @src());
        return error.SDLGLSetAttributeFailed;
    }
    if (c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_MAJOR_VERSION, 4) != 0) {
        logSdlError("SDL_GL_SetAttribute", @src());
        return error.SDLGLSetAttributeFailed;
    }
    if (c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_MINOR_VERSION, 1) != 0) {
        logSdlError("SDL_GL_SetAttribute", @src());
        return error.SDLGLSetAttributeFailed;
    }
    if (c.SDL_GL_SetAttribute(c.SDL_GL_DOUBLEBUFFER, 1) != 0) {
        logSdlError("SDL_GL_SetAttribute", @src());
        return error.SDLGLSetAttributeFailed;
    }

    const window = c.SDL_CreateWindow(
        "Hello World!",
        c.SDL_WINDOWPOS_UNDEFINED, c.SDL_WINDOWPOS_UNDEFINED,
        800, 600,
        c.SDL_WINDOW_ALLOW_HIGHDPI | c.SDL_WINDOW_RESIZABLE | c.SDL_WINDOW_OPENGL,
    ) orelse {
        logSdlError("SDL_CreateWindow", @src());
        return error.SDLCreateWindowFailed;
    };
    defer c.SDL_DestroyWindow(window);

    c.SDL_SetWindowMinimumSize(window, 100, 100);

    var gc = try GraphicsContext.init(gpa, window, &font);
    defer gc.deinit();

    // Disable vsync for better idea of the performance
    if (c.SDL_GL_SetSwapInterval(0) != 0) {
        logSdlError("SDL_GL_SetSwapInterval", @src());
        return error.SDLGLSetSwapInterval;
    }

    const text = try gc.shapeText("The quick brown fox jumps over the lazy dog");
    defer c.hb_buffer_destroy(text);
    const text_2 = try gc.shapeText("THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG");
    defer c.hb_buffer_destroy(text_2);

    var frame_timer = try std.time.Timer.start();
    var frame_time: f64 = 0;
    var frames: u32 = 0;
    var dx: f32 = 0;
    var use_perspective = false;
    var quit = false;
    while (!quit) : ({ dx += 0.1; frames += 1; }) {
        const high_dpi_factor = getHighDpiFactorGL(window);

        if (dx > 80 * high_dpi_factor) dx = 0;

        const current_timer_time = frame_timer.read();
        if (current_timer_time >= std.time.ns_per_s) {
            frame_time = @intToFloat(f64, current_timer_time) / 1000 / 1000 / @intToFloat(f64, frames);
            //std.debug.print("Frame time is {d:.2}ms or {d:.2}fps\n", .{ frame_time,  1000/frame_time });
            frame_timer.reset();
            frames = 0;
        }

        var event: c.SDL_Event = undefined;
        while (c.SDL_PollEvent(&event) != 0) {
            switch (event.type) {
                c.SDL_QUIT => quit = true,
                c.SDL_KEYDOWN => {
                    if (event.key.keysym.sym == c.SDLK_ESCAPE) {
                        quit = true;
                    }
                    if (event.key.keysym.sym == c.SDLK_p) {
                        use_perspective = !use_perspective;
                    }
                },
                else => {},
            }
        }

        var drawable_width_int: i32 = undefined;
        var drawable_height_int: i32 = undefined;
        c.SDL_GL_GetDrawableSize(window, &drawable_width_int, &drawable_height_int);
        const drawable_width = @intToFloat(f32, drawable_width_int);
        const drawable_height = @intToFloat(f32, drawable_height_int);

        gl.clearColor(1, 1, 1, 1);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        {
            const frame_time_text = try std.fmt.allocPrint(gpa, "{d:.2}ms ({d:.2}fps)", .{ frame_time, 1000/frame_time });
            defer gpa.free(frame_time_text);
            const shaped_text = try gc.shapeText(frame_time_text);
            defer c.hb_buffer_destroy(shaped_text);

            const text_size = high_dpi_factor*16;
            var text_width: f32 = 0;
            gc.getTextSize(shaped_text, text_size, &text_width, null);
            const ascender_size = gc.getFontAscenderSize(text_size);

            try gc.drawText(shaped_text, drawable_width - text_width - 25 * high_dpi_factor, drawable_height - ascender_size - 25 * high_dpi_factor, high_dpi_factor*16);
        }

        var i: u32 = 0;
        var y: f32 = drawable_height - 25 * high_dpi_factor;
        while (i < 17) : (i += 1) {
            const text_size = high_dpi_factor * @intToFloat(f32, i + 8);
            const ascender_size = gc.getFontAscenderSize(text_size);
            const descender_size = gc.getFontDescenderSize(text_size);

            const text_size_text = try std.fmt.allocPrint(gpa, "{d}px:", .{ text_size / high_dpi_factor });
            defer gpa.free(text_size_text);
            const shaped_text = try gc.shapeText(text_size_text);
            defer c.hb_buffer_destroy(shaped_text);
            try gc.drawText(shaped_text, 25 * high_dpi_factor, y - ascender_size, text_size);

            try gc.drawText(text, 100 * high_dpi_factor, y - ascender_size, text_size);

            y -= ascender_size - descender_size + 3 * high_dpi_factor;
        }

        try gc.drawText(text, 25 * high_dpi_factor + dx, 25 * high_dpi_factor + dx, high_dpi_factor*16);
        try gc.drawText(text_2, 25 * high_dpi_factor + dx, 40 * high_dpi_factor + dx, high_dpi_factor*16);
        //try gc.drawGlyph(68, 25 * high_dpi_factor + dx, 250 * high_dpi_factor, @intToFloat(f32, font.head_table.units_per_em));

        gc.flush(use_perspective);

        c.SDL_GL_SwapWindow(window);
    }
}

