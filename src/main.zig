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
    tag     : [4]u8,
    checksum: u32,
    offset  : u32,
    length  : u32,
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

    if (!offset_subtable.isTrueTypeFont()) {
        std.debug.print("Not a valid TrueType Font\n", .{});
        return error.InvalidFontFile;
    }

    var table_directory = try arena.alloc(TableDirectoryEntry, offset_subtable.num_tables);
    for (table_directory) |*entry| {
        entry.* = try reader.readStruct(TableDirectoryEntry);
        bswapAllIntFieldsToHost(TableDirectoryEntry, entry);
        std.debug.print("{s}: {}\n", .{ entry.tag, entry });
    }
}

