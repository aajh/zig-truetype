const std = @import("std");
const builtin = @import("builtin");

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

fn getTableDirectoryEntry(entries: []TableDirectoryEntry, tag: *const [TableDirectoryEntry.TAG_LENGTH]u8) ?TableDirectoryEntry {
    for (entries) |entry| {
        if (std.mem.eql(u8, entry.tag[0..entry.tag.len], tag)) {
            return entry;
        }
    }
    return null;
}

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

    gpa             : std.mem.Allocator,
    end_codes       : []u16,
    start_codes     : []u16,
    id_deltas       : []u16,
    id_range_offsets: []u16, // Processed to be an index to the glyph_indices array
    glyph_indices   : []u16,

    fn init(gpa: std.mem.Allocator, seekable_stream: anytype, reader: anytype) !CmapFormat4 {
        var cmap: CmapFormat4 = undefined;
        cmap.gpa = gpa;

        const cmap_offset = try seekable_stream.getPos();

        var header = try reader.readStruct(Header);
        bswapAllIntFieldsToHost(Header, &header);
        const seg_count = header.seg_count_x2 / 2;
        std.debug.print("cmap header: {}\n", .{ header });
        std.debug.print("seg_count: {}\n", .{ seg_count });

        if (header.format != 4) {
            std.debug.print("Trying to read wrong format cmap table\n (expected 4 got {d})\n", .{ header.format });
            return error.ParseError;
        }

        cmap.end_codes = try gpa.alloc(u16, seg_count);
        errdefer gpa.free(cmap.end_codes);
        for (cmap.end_codes) |*end_code| {
            end_code.* = try reader.readIntBig(u16);
        }
        std.debug.print("end_codes: {any}\n", .{ cmap.end_codes });

        if ((try reader.readIntBig(u16)) != 0) {
            std.debug.print("Invalid cmap table\n", .{});
            return error.InvalidFontFile;
        }

        cmap.start_codes = try gpa.alloc(u16, seg_count);
        errdefer gpa.free(cmap.start_codes);
        for (cmap.start_codes) |*start_code| {
            start_code.* = try reader.readIntBig(u16);
        }
        std.debug.print("start_codes: {any}\n", .{ cmap.start_codes });

        cmap.id_deltas = try gpa.alloc(u16, seg_count);
        errdefer gpa.free(cmap.id_deltas);
        for (cmap.id_deltas) |*id_delta| {
            id_delta.* = try reader.readIntBig(u16);
        }
        std.debug.print("id_deltas: {any}\n", .{ cmap.id_deltas });

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
        std.debug.print("id_range_offsets: {any}\n", .{ cmap.id_range_offsets });

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
        std.debug.print("glyph_indices: {any}\n", .{ cmap.glyph_indices });

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
        const c = @intCast(u16, codepoint);

        var segment_index: i32 = -1;
        for (cmap.end_codes) |end_code, i| {
            if (c <= end_code) {
                segment_index = @intCast(i32, i);
                break;
            }
        }
        if (segment_index == -1) {
            std.debug.print("cmap segment not found for codepoint {}\n", .{ c });
            return 0;
        }
        std.debug.print("{d}\n", .{ segment_index });
        const i = @intCast(usize, segment_index);

        std.debug.print("cmap segment for codepoint {}: end {}, start {}, id_delta {}, id_range_offset {}\n", .{ c, cmap.end_codes[i], cmap.start_codes[i], cmap.id_deltas[i], cmap.id_range_offsets[i] });

        if (cmap.id_range_offsets[i] != 0) {
            // TODO: Test this code
            const index_to_glyph_indices = cmap.id_range_offsets[i] + (c - cmap.start_codes[i]);
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

        return @intCast(u16, (@intCast(u32, cmap.id_deltas[i]) + @intCast(u32, c)) & 0xFFFF);
    }
};

pub fn main() !void {
    var arena_instance = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_instance.deinit();
    var arena = arena_instance.allocator();

    var font_file = try std.fs.cwd().openFile("AvenirNext-Regular-08.ttf", .{ .read = true });
    //var font_file = try std.fs.cwd().openFile("Komrade-Regular.otf", .{ .read = true });
    defer font_file.close();

    var reader = font_file.reader();

    var offset_subtable = try reader.readStruct(OffsetSubtable);
    bswapAllIntFieldsToHost(OffsetSubtable, &offset_subtable);
    std.debug.print("{}\n", .{ offset_subtable });
    std.debug.print("isTrueTypeFont: {}\n", .{ offset_subtable.isTrueTypeFont() });
    std.debug.print("isOpenTypeFont: {}\n", .{ offset_subtable.isOpenTypeFont() });
    std.debug.print("\n", .{});

    if (!offset_subtable.isTrueTypeFont()) {
        std.debug.print("Not a valid TrueType Font\n", .{});
        return error.InvalidFontFile;
    }

    if (offset_subtable.num_tables > 32) {
        std.debug.print("Too many tables in the font file\n", .{});
        return error.InvalidFontFile;
    }

    var table_directory = try arena.alloc(TableDirectoryEntry, offset_subtable.num_tables);
    for (table_directory) |*entry| {
        entry.* = try reader.readStruct(TableDirectoryEntry);
        bswapAllIntFieldsToHost(TableDirectoryEntry, entry);
        std.debug.print("{s}: {}\n", .{ entry.tag, entry });
    }


    const head_table_directory_entry = getTableDirectoryEntry(table_directory, "head") orelse {
        std.debug.print("The TrueType font file didn't include \"head\" table\n", .{});
        return error.InvalidFontFile;
    };
    try font_file.seekTo(head_table_directory_entry.offset);
    var head_table = try reader.readStruct(HeadTable);
    bswapAllIntFieldsToHost(HeadTable, &head_table);

    std.debug.print("\n", .{});
    std.debug.print("{}\n", .{ head_table });
    std.debug.print("HeadTable isValid {}\n", .{ head_table.isValid() });


    const maxp_table_directory_entry = getTableDirectoryEntry(table_directory, "maxp") orelse {
        std.debug.print("The TrueType font file didn't include \"maxp\" table\n", .{});
        return error.InvalidFontFile;
    };
    try font_file.seekTo(maxp_table_directory_entry.offset);
    var maxp_table = try reader.readStruct(MaxpTable);
    bswapAllIntFieldsToHost(MaxpTable, &maxp_table);
    std.debug.print("\n", .{});
    std.debug.print("{}\n", .{ maxp_table });


    const cmap_table_directory_entry = getTableDirectoryEntry(table_directory, "cmap") orelse {
        std.debug.print("The TrueType font file didn't include \"cmap\" table\n", .{});
        return error.InvalidFontFile;
    };
    try font_file.seekTo(cmap_table_directory_entry.offset);
    var cmap_table = try reader.readStruct(CmapTable);
    bswapAllIntFieldsToHost(CmapTable, &cmap_table);
    std.debug.print("\n", .{});
    std.debug.print("{}\n", .{ cmap_table });
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
        std.debug.print("{}\n", .{ subtable });

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
    std.debug.print("Selected cmap table: {}\n", .{ selected_cmap_subtable });

    const cmap_offset = cmap_table_directory_entry.offset + selected_cmap_subtable.offset;
    try font_file.seekTo(cmap_offset);
    const cmap_format = try reader.readIntBig(u16);
    std.debug.print("Selected cmap table has format {d}\n", .{ cmap_format });
    if (cmap_format != 4) {
        std.debug.print("Unsupported cmap table format\n", .{});
        return error.UnsupportedFontFile;
    }

    try font_file.seekBy(-@sizeOf(u16));
    var cmap = try CmapFormat4.init(arena, font_file.seekableStream(), reader);
    defer cmap.deinit();
    const codepoint = 'A';
    const glyph_index = cmap.codepointToGlyphIndex(codepoint);
    std.debug.print("Codepoint {c} has glyph index {d}\n", .{ codepoint, glyph_index });
}

