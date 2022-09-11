const std = @import("std");
const builtin = @import("builtin");
const c = @import("c.zig");

const Allocator = std.mem.Allocator;
const Vector = std.meta.Vector;

const native_endian = builtin.cpu.arch.endian();

fn bswapAllIntFieldsToHost(comptime S: type, ptr: *S) void {
    if (@typeInfo(S) != .Struct) @compileError("bswapAllFields expects a struct as the first argument");
    if (native_endian == .Big) return;
    inline for (std.meta.fields(S)) |f| {
        if (@typeInfo(f.field_type) == .Int) {
            @field(ptr, f.name) = @byteSwap(f.field_type, @field(ptr, f.name));
        }
    }
}

const OffsetSubtable = packed struct {
    scaler_type   : u32,
    num_tables    : u16,
    searchRange   : u16,
    entry_selector: u16,
    range_shift   : u16,

    fn isTrueTypeFont(table: OffsetSubtable) bool {
        return table.scaler_type == 0x00010000 or table.scaler_type == std.mem.readVarInt(u32, "true", .Big);
    }

    fn isOpenTypeFont(table: OffsetSubtable) bool {
        return table.scaler_type == std.mem.readVarInt(u32, "OTTO", .Big);
    }
};

const TableDirectoryEntry = packed struct {
    const TAG_LENGTH = 4;
    tag     : [TAG_LENGTH]u8,
    checksum: u32,
    offset  : u32,
    length  : u32,
};

const HeadTable = packed struct {
    version            : u32,
    font_revision      : u32,
    checksum_adjustment: u32,
    magic_number       : u32,
    flags              : u16,
    units_per_em       : u16,
    created            : i64,
    modified           : i64,
    x_min              : i16,
    y_min              : i16,
    x_max              : i16,
    y_max              : i16,
    mac_style          : u16,
    lowest_rec_ppem    : u16,
    font_direction_hint: i16,
    index_to_loc_format: i16,
    glyph_data_format  : i16,

    fn isValid(table: HeadTable) bool {
        return table.version == 0x00010000 and table.magic_number == 0x5F0F3CF5;
    }
};

const MaxpTable = packed struct {
    version: u32,
    num_glyphs: u16,
};

const HheaTable = packed struct {
    version                : u32,
    ascent                 : i16,
    descent                : i16,
    line_gap               : i16,
    advance_width_max      : u16,
    min_left_side_bearing  : i16,
    min_right_side_bearing : i16,
    x_max_extent           : i16,
    caret_slope_rise       : i16,
    carte_slope_run        : i16,
    caret_offset           : i16,
    reserved               : i64,
    metric_data_format     : i16,
    num_of_long_hor_metrics: u16,
};

const CmapTable = packed struct {
    version      : u16,
    num_subtables: u16,

    fn isValid(table: CmapTable) bool {
        return table.version == 0;
    }
};

const CmapSubtable = packed struct {
    platform_id         : u16,
    platform_specific_id: u16,
    offset              : u32,
};

const CmapFormat4 = struct {
    const Header = packed struct {
        format        : u16,
        length        : u16,
        language      : u16,
        seg_count_x2  : u16,
        search_range  : u16,
        entry_selector: u16,
        range_shift   : u16,
    };

    gpa             : Allocator,
    end_codes       : []u16,
    start_codes     : []u16,
    id_deltas       : []u16,
    id_range_offsets: []u16, // Processed to be an index to the glyph_indices array
    glyph_indices   : []u16,

    fn init(gpa: Allocator, seekable_stream: anytype, reader: anytype) !CmapFormat4 {
        var cmap: CmapFormat4 = undefined;
        cmap.gpa = gpa;

        const cmap_offset = try seekable_stream.getPos();

        var header = try reader.readStruct(Header);
        bswapAllIntFieldsToHost(Header, &header);
        const seg_count = header.seg_count_x2 / 2;
        //std.debug.print("cmap header: {}\n", .{ header });
        //std.debug.print("seg_count: {}\n", .{ seg_count });

        if (header.format != 4) {
            std.debug.print("Trying to read wrong format cmap table\n (expected 4 got {d})\n", .{ header.format });
            return error.ParseError;
        }

        cmap.end_codes = try gpa.alloc(u16, seg_count);
        errdefer gpa.free(cmap.end_codes);
        for (cmap.end_codes) |*end_code| {
            end_code.* = try reader.readIntBig(u16);
        }
        //std.debug.print("end_codes: {any}\n", .{ cmap.end_codes });

        if ((try reader.readIntBig(u16)) != 0) {
            std.debug.print("Invalid cmap table\n", .{});
            return error.InvalidFontFile;
        }

        cmap.start_codes = try gpa.alloc(u16, seg_count);
        errdefer gpa.free(cmap.start_codes);
        for (cmap.start_codes) |*start_code| {
            start_code.* = try reader.readIntBig(u16);
        }
        //std.debug.print("start_codes: {any}\n", .{ cmap.start_codes });

        cmap.id_deltas = try gpa.alloc(u16, seg_count);
        errdefer gpa.free(cmap.id_deltas);
        for (cmap.id_deltas) |*id_delta| {
            id_delta.* = try reader.readIntBig(u16);
        }
        //std.debug.print("id_deltas: {any}\n", .{ cmap.id_deltas });

        cmap.id_range_offsets = try gpa.alloc(u16, seg_count);
        errdefer gpa.free(cmap.id_range_offsets);
        for (cmap.id_range_offsets) |*id_range_offset, i| {
            id_range_offset.* = try reader.readIntBig(u16);
            if (id_range_offset.* != 0) {
                // TODO: Test this code.
                const offset_to_glyph_indices_start = (seg_count - @intCast(u16, i)) * @sizeOf(u16);
                id_range_offset.* = (id_range_offset.* - offset_to_glyph_indices_start) / @sizeOf(u16);
            }
        }
        //std.debug.print("id_range_offsets: {any}\n", .{ cmap.id_range_offsets });

        const glyph_indices_start_pos = try seekable_stream.getPos();
        const glyph_indices_len = header.length - (glyph_indices_start_pos - cmap_offset);
        if (glyph_indices_len % 2 != 0) {
            std.debug.print("Invalid glyph_indices length {d} (should be divisable by two)\n", .{ glyph_indices_len });
            return error.InvalidFontFile;
        }
        cmap.glyph_indices = try gpa.alloc(u16, glyph_indices_len / 2);
        errdefer gpa.free(cmap.glyph_indices);
        for (cmap.glyph_indices) |*glyph_index| {
            glyph_index.* = try reader.readIntBig(u16);
        }
        //std.debug.print("glyph_indices: {any}\n", .{ cmap.glyph_indices });

        return cmap;
    }

    fn deinit(cmap: *CmapFormat4) void {
        cmap.gpa.free(cmap.end_codes);
        cmap.gpa.free(cmap.start_codes);
        cmap.gpa.free(cmap.id_deltas);
        cmap.gpa.free(cmap.id_range_offsets);
        cmap.gpa.free(cmap.glyph_indices);
    }

    fn codepointToGlyphIndex(cmap: CmapFormat4, codepoint: u21) u16 {
        if (codepoint > std.math.maxInt(u16)) {
            return 0;
        }
        const cp = @intCast(u16, codepoint);

        var segment_index: i32 = -1;
        for (cmap.end_codes) |end_code, i| {
            if (cp <= end_code) {
                segment_index = @intCast(i32, i);
                break;
            }
        }
        if (segment_index == -1) {
            std.debug.print("cmap segment not found for codepoint {}\n", .{ cp });
            return 0;
        }
        //std.debug.print("{d}\n", .{ segment_index });
        const i = @intCast(usize, segment_index);

        //std.debug.print("cmap segment for codepoint {}: end {}, start {}, id_delta {}, id_range_offset {}\n", .{ cp, cmap.end_codes[i], cmap.start_codes[i], cmap.id_deltas[i], cmap.id_range_offsets[i] });

        if (cmap.id_range_offsets[i] != 0) {
            // TODO: Test this code
            const index_to_glyph_indices = cmap.id_range_offsets[i] + (cp - cmap.start_codes[i]);
            if (index_to_glyph_indices >= cmap.glyph_indices.len) {
                std.debug.print("cmap table resulted in an invalid index to glyph indices array\n", .{});
                return 0;
            }

            const glyph_index = cmap.glyph_indices[index_to_glyph_indices];
            if (glyph_index != 0) {
                return @intCast(u16, (@intCast(u32, cmap.id_deltas[i]) + @intCast(u32, glyph_index)) & 0xFFFF);
            } else {
                return 0;
            }
        }

        return @intCast(u16, (@intCast(u32, cmap.id_deltas[i]) + @intCast(u32, cp)) & 0xFFFF);
    }
};

const LocaTable = struct {
    const OffsetsUnion = union(enum) {
        short: []u16,
        long : []u32,
    };

    gpa: Allocator,
    offsets: OffsetsUnion,

    fn init(gpa: Allocator, loca_table_directory_entry: TableDirectoryEntry, head_table: HeadTable, seekable_stream: anytype, reader: anytype) !LocaTable {
        try seekable_stream.seekTo(loca_table_directory_entry.offset);
        const length = loca_table_directory_entry.length;

        const offsets = switch (head_table.index_to_loc_format) {
            0 => blk: {
                var os = try gpa.alloc(u16, length / @sizeOf(u16));
                for (os) |*o| {
                    o.* = try reader.readIntBig(u16);
                }
                break :blk OffsetsUnion{
                    .short = os,
                };
            },
            1 => blk: {
                var os = try gpa.alloc(u32, length / @sizeOf(u32));
                for (os) |*o| {
                    o.* = try reader.readIntBig(u32);
                }
                break :blk OffsetsUnion{
                    .long = os,
                };
            },
            else => {
                std.debug.print("Invalid index_to_loc_format: expected 0 or 1, got {d}\n", .{ head_table.index_to_loc_format });
                return error.InvalidFontFile;
            }
        };

        return LocaTable{
            .gpa = gpa,
            .offsets = offsets,
        };
    }

    fn deinit(loca: LocaTable) void {
        switch (loca.offsets) {
            .short => |offsets| loca.gpa.free(offsets),
            .long  => |offsets| loca.gpa.free(offsets),
        }
    }

    const GlyphLocation = struct {
        offset: u32,
        length: u32,
    };

    fn glyphIndexToLocation(loca: LocaTable, glyph: u32) !GlyphLocation {
        switch (loca.offsets) {
            .short => |offsets| {
                if (glyph + 1 >= offsets.len) {
                    return error.GlyphNotFound;
                }

                return GlyphLocation{
                    .offset = offsets[glyph],
                    .length = offsets[glyph + 1] - offsets[glyph],
                };
            },
            .long => |offsets| {
                if (glyph + 1 >= offsets.len) {
                    return error.GlyphNotFound;
                }

                return GlyphLocation{
                    .offset = offsets[glyph],
                    .length = offsets[glyph + 1] - offsets[glyph],
                };
            },
        }
    }
};

pub const Font = struct {
    gpa              : Allocator,
    font_file_content: []const u8,

    table_directory  : []TableDirectoryEntry,
    head_table       : HeadTable   = undefined,
    hhea_table       : HheaTable   = undefined,
    cmap             : CmapFormat4 = undefined,
    loca_table       : LocaTable   = undefined,
    glyf_table_offset: u64         = undefined,

    const MAX_FONT_FILE_SIZE = 100 * 1024 * 1024;

    pub fn initWithFile(gpa: Allocator, font_file: std.fs.File) !Font {
        const font_file_content = try font_file.readToEndAlloc(gpa, MAX_FONT_FILE_SIZE);

        var stream = std.io.fixedBufferStream(font_file_content);
        var reader = stream.reader();
        var seekable_stream = stream.seekableStream();

        var offset_subtable = try reader.readStruct(OffsetSubtable);
        bswapAllIntFieldsToHost(OffsetSubtable, &offset_subtable);
        //std.debug.print("{}\n", .{ offset_subtable });
        //std.debug.print("isTrueTypeFont: {}\n", .{ offset_subtable.isTrueTypeFont() });
        //std.debug.print("isOpenTypeFont: {}\n", .{ offset_subtable.isOpenTypeFont() });
        //std.debug.print("\n", .{});

        if (!offset_subtable.isTrueTypeFont()) {
            std.debug.print("Not a valid TrueType Font\n", .{});
            return error.InvalidFontFile;
        }

        if (offset_subtable.num_tables > 32) {
            std.debug.print("Too many tables in the font file\n", .{});
            return error.InvalidFontFile;
        }

        var table_directory = try gpa.alloc(TableDirectoryEntry, offset_subtable.num_tables);
        errdefer gpa.free(table_directory);
        for (table_directory) |*entry| {
            entry.* = try reader.readStruct(TableDirectoryEntry);
            bswapAllIntFieldsToHost(TableDirectoryEntry, entry);
            //std.debug.print("{s}: {}\n", .{ entry.tag, entry });
        }

        var font = Font{
            .gpa = gpa,
            .font_file_content = font_file_content,
            .table_directory = table_directory,
        };


        const head_table_directory_entry = font.getTableDirectoryEntry("head") orelse {
            std.debug.print("The TrueType font file didn't include \"head\" table\n", .{});
            return error.InvalidFontFile;
        };
        try seekable_stream.seekTo(head_table_directory_entry.offset);
        font.head_table = try reader.readStruct(HeadTable);
        bswapAllIntFieldsToHost(HeadTable, &font.head_table);

        //std.debug.print("\n", .{});
        //std.debug.print("{}\n", .{ font.head_table });
        //std.debug.print("HeadTable isValid {}\n", .{ font.head_table.isValid() });


        //const maxp_table_directory_entry = font.getTableDirectoryEntry("maxp") orelse {
            //std.debug.print("The TrueType font file didn't include \"maxp\" table\n", .{});
            //return error.InvalidFontFile;
        //};
        //try seekable_stream.seekTo(maxp_table_directory_entry.offset);
        //var maxp_table = try reader.readStruct(MaxpTable);
        //bswapAllIntFieldsToHost(MaxpTable, &maxp_table);
        //std.debug.print("\n", .{});
        //std.debug.print("{}\n", .{ maxp_table });


        const hhea_table_directory_entry = font.getTableDirectoryEntry("hhea") orelse {
            std.debug.print("The TrueType font file didn't include \"hhea\" table\n", .{});
            return error.InvalidFontFile;
        };
        try seekable_stream.seekTo(hhea_table_directory_entry.offset);
        font.hhea_table = try reader.readStruct(HheaTable);
        bswapAllIntFieldsToHost(HheaTable, &font.hhea_table);
        //std.debug.print("\n", .{});
        //std.debug.print("{}\n", .{ font.hhea_table });


        const cmap_table_directory_entry = font.getTableDirectoryEntry("cmap") orelse {
            std.debug.print("The TrueType font file didn't include \"cmap\" table\n", .{});
            return error.InvalidFontFile;
        };
        try seekable_stream.seekTo(cmap_table_directory_entry.offset);
        var cmap_table = try reader.readStruct(CmapTable);
        bswapAllIntFieldsToHost(CmapTable, &cmap_table);
        //std.debug.print("\n", .{});
        //std.debug.print("{}\n", .{ cmap_table });
        if (!cmap_table.isValid()) {
            std.debug.print("The cmap table was invalid\n", .{});
            return error.InvalidFontFile;
        }

        const INVALID_PLATFORM_ID: u16 = 0xFFFF;
        var selected_cmap_subtable: CmapSubtable = .{
            .platform_id = INVALID_PLATFORM_ID,
            .platform_specific_id = 0xFFFF,
            .offset = 0,
        };
        var cmap_subtable_i: u16 = 0;
        while (cmap_subtable_i < cmap_table.num_subtables) : (cmap_subtable_i += 1) {
            var subtable = try reader.readStruct(CmapSubtable);
            bswapAllIntFieldsToHost(CmapSubtable, &subtable);
            //std.debug.print("{}\n", .{ subtable });

            if (subtable.platform_id == 0) {
                if (selected_cmap_subtable.platform_id == INVALID_PLATFORM_ID) {
                    selected_cmap_subtable = subtable;
                } else if (selected_cmap_subtable.platform_specific_id != 4) {
                    selected_cmap_subtable = subtable;
                }
            }
        }
        if (selected_cmap_subtable.platform_id == INVALID_PLATFORM_ID) {
            std.debug.print("Couldn't find a Unicode cmap table\n", .{});
            return error.UnsupportedFontFile;
        }
        //std.debug.print("Selected cmap table: {}\n", .{ selected_cmap_subtable });

        const cmap_offset = cmap_table_directory_entry.offset + selected_cmap_subtable.offset;
        try seekable_stream.seekTo(cmap_offset);
        const cmap_format = try reader.readIntBig(u16);
        //std.debug.print("Selected cmap table has format {d}\n", .{ cmap_format });
        if (cmap_format != 4) {
            std.debug.print("Unsupported cmap table format {d}\n", .{ cmap_format });
            return error.UnsupportedFontFile;
        }

        try seekable_stream.seekBy(-@sizeOf(u16));
        font.cmap = try CmapFormat4.init(gpa, seekable_stream, reader);
        errdefer font.cmap.deinit();


        const loca_table_directory_entry = font.getTableDirectoryEntry("loca") orelse {
            std.debug.print("The TrueType font file didn't include \"loca\" table\n", .{});
            return error.InvalidFontFile;
        };
        font.loca_table = try LocaTable.init(gpa, loca_table_directory_entry, font.head_table, seekable_stream, reader);
        errdefer font.loca_table.deinit();

        const glyf_table_directory_entry = font.getTableDirectoryEntry("glyf") orelse {
            std.debug.print("The TrueType font file didn't include \"glyf\" table\n", .{});
            return error.InvalidFontFile;
        };
        font.glyf_table_offset = glyf_table_directory_entry.offset;

        return font;
    }

    pub fn deinit(font: *Font) void {
        font.loca_table.deinit();
        font.cmap.deinit();
        font.gpa.free(font.table_directory);
        font.gpa.free(font.font_file_content);
    }

    fn getTableDirectoryEntry(font: Font, tag: *const [TableDirectoryEntry.TAG_LENGTH]u8) ?TableDirectoryEntry {
        for (font.table_directory) |entry| {
            if (std.mem.eql(u8, entry.tag[0..entry.tag.len], tag)) {
                return entry;
            }
        }
        return null;
    }

    pub fn getGlyph(font: Font, glyph_index: u32) !?Glyph {
        const glyph_location = try font.loca_table.glyphIndexToLocation(glyph_index);
        //std.debug.print("The glyph has offset and length {}\n", .{ glyph_location });

        if (glyph_location.length == 0) return null;

        var stream = std.io.fixedBufferStream(font.font_file_content);
        var reader = stream.reader();
        var seekable_stream = stream.seekableStream();

        try seekable_stream.seekTo(font.glyf_table_offset + glyph_location.offset);
        return try Glyph.init(font.gpa, reader);
    }
};

fn readCoordinates(reader: anytype, flags: []u8, coordinates: []i16, IS_SHORT: u8, SAME_OR_POSITIVE: u8) !void {
    std.debug.assert(flags.len == coordinates.len);

    var previous_coordinate: i16 = 0;
    for (flags) |flag, i| {
        var delta_coordinate: i16 = 0;
        if (flag & IS_SHORT > 0) {
            delta_coordinate = try reader.readIntBig(u8);
            if (flag & SAME_OR_POSITIVE == 0) delta_coordinate *= -1;
        } else if (flag & SAME_OR_POSITIVE == 0) {
            delta_coordinate = try reader.readIntBig(i16);
        }
        previous_coordinate += delta_coordinate;
        coordinates[i] = previous_coordinate;
    }
}

const Point = Vector(2, i16);

pub const QuadraticBezierCurve = packed struct {
    p0: Point,
    p1: Point,
    p2: Point,
};

const Glyph = struct {
    gpa: Allocator,

    x_min: i16,
    y_min: i16,
    x_max: i16,
    y_max: i16,

    segments: []QuadraticBezierCurve,

    fn init(gpa: Allocator, reader: anytype) !Glyph {
        const number_of_contours_i16 = try reader.readIntBig(i16);
        //std.debug.print("The glyph has number_of_contours of {d}\n", .{ number_of_contours_i16 });
        if (number_of_contours_i16 < 0) {
            std.debug.print("The glyph is a compound glyph, which are not supported\n", .{});
            return error.UnsupportedGlyph;
        }
        const number_of_contours = @intCast(u16, number_of_contours_i16);

        const x_min = try reader.readIntBig(i16);
        const y_min = try reader.readIntBig(i16);
        const x_max = try reader.readIntBig(i16);
        const y_max = try reader.readIntBig(i16);
        //std.debug.print("x_min {d}, y_min {d}, x_max {d}, y_max {d}\n", .{ x_min, y_min, x_max, y_max });

        var end_points_of_contours = try gpa.alloc(u16, number_of_contours);
        defer gpa.free(end_points_of_contours);
        var points_length: u16 = 0;
        for (end_points_of_contours) |*end_point| {
            end_point.* = try reader.readIntBig(u16);
            if (end_point.* > points_length) points_length = end_point.*;
        }
        if (number_of_contours > 0) {
            points_length += 1;
        }
        //std.debug.print("End points of contours: {any}, points_length: {d}\n", .{ end_points_of_contours, points_length });

        const instruction_length = try reader.readIntBig(u16);
        try reader.skipBytes(instruction_length * @sizeOf(u8), .{});

        const ON_CURVE          : u8 = 0b00000001;
        const X_IS_SHORT        : u8 = 0b00000010;
        const Y_IS_SHORT        : u8 = 0b00000100;
        const REPEAT            : u8 = 0b00001000;
        const X_SAME_OR_POSITIVE: u8 = 0b00010000;
        const Y_SAME_OR_POSITIVE: u8 = 0b00100000;
        var flags = try gpa.alloc(u8, points_length);
        defer gpa.free(flags);
        var flag_i: u16 = 0;
        while (flag_i < points_length) : (flag_i += 1) {
            const flag = try reader.readIntBig(u8);
            flags[flag_i] = flag;

            if (flag & REPEAT > 0) {
                var repeat_count = try reader.readIntBig(u8);
                while (repeat_count > 0) : (repeat_count -= 1) {
                    flag_i += 1;
                    if (flag_i >= points_length) {
                        return error.InvalidGlyph;
                    }
                    flags[flag_i] = flag;
                }
            }
        }
        //std.debug.print("Glyph flags: {any}\n", .{ flags });

        var x_coordinates: []i16 = try gpa.alloc(i16, points_length);
        defer gpa.free(x_coordinates);
        try readCoordinates(reader, flags, x_coordinates, X_IS_SHORT, X_SAME_OR_POSITIVE);
        //std.debug.print("x_coordinates: {any}\n", .{ x_coordinates });

        var y_coordinates: []i16 = try gpa.alloc(i16, points_length);
        defer gpa.free(y_coordinates);
        try readCoordinates(reader, flags, y_coordinates, Y_IS_SHORT, Y_SAME_OR_POSITIVE);
        //std.debug.print("y_coordinates: {any}\n", .{ y_coordinates });

        var last_contour_end: u16 = 0;
        var segment_count: u16 = 0;
        var state: enum {
            start,
            on_curve, // The next point is the end of a straight line or a control point
            off_curve, // The next point is the end of bezier curves or another control point
        } = .start;
        var contour_i: u16        = 0;
        while (contour_i < number_of_contours) : (contour_i += 1) {
            const contour_end = end_points_of_contours[contour_i] + 1;
            defer last_contour_end = contour_end;

            state = .start;
            var point_i: u16        = last_contour_end;
            while (point_i < contour_end) : (point_i += 1) {
                const on_curve = flags[point_i] & ON_CURVE > 0;

                switch (state) {
                    .start => {
                        state = .on_curve;
                    },
                    .on_curve => {
                        if (on_curve) {
                            segment_count += 1;
                        } else {
                            state = .off_curve;
                        }
                    },
                    .off_curve => {
                        segment_count += 1;
                        if (on_curve) {
                            state = .on_curve;
                        }
                    }
                }
            }
            segment_count += 1; // 1 is for the segment that closes the contour
        }

        var segments = try gpa.alloc(QuadraticBezierCurve, segment_count);
        errdefer gpa.free(segments);

        var segment_i: u16 = 0;
        last_contour_end   = 0;
        contour_i          = 0;
        while (contour_i < number_of_contours) : (contour_i += 1) {
            const contour_end = end_points_of_contours[contour_i] + 1;
            defer last_contour_end = contour_end;

            var first_point          = Point{};
            var first_point_on_curve = false;
            var last_point           = Point{};
            var last_on_curve_point  = Point{};
            state                    = .start;
            var point_i: u16         = last_contour_end;
            while (point_i < contour_end) : (point_i += 1) {
                const on_curve = flags[point_i] & ON_CURVE > 0;
                const point = Point{
                    //@intToFloat(f32, x_coordinates[point_i]),
                    //@intToFloat(f32, y_coordinates[point_i]),
                    x_coordinates[point_i],
                    y_coordinates[point_i],
                };

                switch (state) {
                    .start => {
                        first_point = point;
                        first_point_on_curve = on_curve;
                        state = .on_curve;
                    },
                    .on_curve => {
                        if (on_curve) {
                            segments[segment_i] = .{
                                .p0 = last_on_curve_point,
                                //.p1 = (last_on_curve_point + point) / @splat(2, @as(f32, 2)),
                                .p1 = last_on_curve_point,
                                .p2 = point,
                            };
                            segment_i += 1;
                        } else {
                            state = .off_curve;
                        }
                    },
                    .off_curve => {
                        if (on_curve) {
                            segments[segment_i] = .{
                                .p0 = last_on_curve_point,
                                .p1 = last_point,
                                .p2 = point,
                            };
                            segment_i += 1;
                            state = .on_curve;
                        } else {
                            //const end = (last_point + point) / @splat(2, @as(f32, 2));
                            const end = @divTrunc(last_point + point, @splat(2, @as(i16, 2)));
                            segments[segment_i] = .{
                                .p0 = last_on_curve_point,
                                .p1 = last_point,
                                .p2 = end,
                            };
                            segment_i += 1;
                            last_on_curve_point = end;
                        }
                    }
                }

                last_point = point;
                if (on_curve) {
                    last_on_curve_point = point;
                }

                if (point_i == contour_end - 1) {
                    // Close the contour
                    switch (state) {
                        .start => {},
                        .on_curve => {
                            segments[segment_i] = .{
                                .p0 = point,
                                //.p1 = (point + first_point) / @splat(2, @as(f32, 2)),
                                .p1 = point,
                                .p2 = first_point,
                            };
                            segment_i += 1;
                        },
                        .off_curve => {
                            if (first_point_on_curve) {
                                segments[segment_i] = .{
                                    .p0 = last_on_curve_point,
                                    .p1 = last_point,
                                    .p2 = first_point,
                                };
                                segment_i += 1;
                            } else {
                                //const end = (last_point + first_point) / @splat(2, @as(f32, 2));
                                const end = @divTrunc(last_point + first_point, @splat(2, @as(i16, 2)));
                                segments[segment_i] = .{
                                    .p0 = last_on_curve_point,
                                    .p1 = last_point,
                                    .p2 = end,
                                };
                                segment_i += 1;
                            }
                        }
                    }
                }
            }
        }

        return Glyph{
            .gpa = gpa,
            .x_min = x_min,
            .y_min = y_min,
            .x_max = x_max,
            .y_max = y_max,
            .segments = segments,
        };
    }

    pub fn deinit(glyph: Glyph) void {
        glyph.gpa.free(glyph.segments);
    }
};

