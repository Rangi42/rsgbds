# This file gets embedded directly into the executables, via `#![debugger_visualizer()]` attributes.

from typing import final
import gdb
import re

ARRAY_VEC_REGEX = re.compile(r"^(arrayvec::(\w+::)+)ArrayVec<.+>$")


@final
class ArrayVecPrinter(gdb.ValuePrinter):
    "Print an arrayvec::arrayvec::ArrayVec"

    def __init__(self, val: gdb.Value):
        self.__length = int(val["len"])
        self.__array = val["xs"]

    def to_string(self):
        return f"ArrayVec(size={self.__length})"

    def children(self):
        for i in range(self.__length):
            # Deref the `MaybeUninit` and the `ManuallyDrop` inside.
            yield f"[{i}]", self.__array[i]["value"]["value"]

    def num_children(self):
        return self.__length

    @staticmethod
    def display_hint():
        return "array"


ARIADNE_SOURCE_REGEX = re.compile(r"^(ariadne::(\w+::)+)Source<.+>$")


@final
class CompactStringPrinter(gdb.ValuePrinter):
    "Print a compact_str::CompactString"

    def __init__(self, val: gdb.Value) -> None:
        repr = val["__0"]
        last_byte = int(repr["__5"])
        if (
            last_byte == 216  # Heap.
            or last_byte == 217  # Static.
        ):
            ptr = repr["__0"].cast(gdb.lookup_type("u8").pointer())
            self.__str = ptr.lazy_string("utf-8", length=int(repr["__1"]))
        else:
            length = last_byte - (128 + 64)
            if length < 0:  # Last byte is a UTF-8 char.
                length = 24
            self.__str = repr.bytes[:length].decode("utf-8")

    def to_string(self):
        return self.__str

    @staticmethod
    def display_hint():
        return "string"


@final
class AriadneSourcePrinter(gdb.ValuePrinter):
    "Print an ariadne::source::Source"

    def __init__(self, val: gdb.Value) -> None:
        self.__text = val["text"]

    def to_string(self):
        return self.__text

    @staticmethod
    def display_hint():
        return "string"


@final
class ColorGroupPrinter(gdb.ValuePrinter):
    def __init__(self, val: gdb.Value):
        array = val["colors"]
        self.__length = int(array["len"])
        self.__array = array["xs"]

    def to_string(self):
        return f"ColorSet(size={self.__length})"

    def children(self):
        for i in range(self.__length):
            # Deref the `MaybeUninit` and the `ManuallyDrop` inside.
            yield f"[{i}]", self.__array[i]["value"]["value"]

    def num_children(self):
        return self.__length

    @staticmethod
    def display_hint():
        return "array"


@final
class Rgb16Printer(gdb.ValuePrinter):
    def __init__(self, val: gdb.Value):
        raw = val["__0"]
        self.__red = raw & 0x1F
        self.__green = raw >> 5 & 0x1F
        self.__blue = raw >> 10 & 0x1F
        self.__alpha = raw >> 15

    def to_string(self):
        return f"Rgb16(r={self.__red.format_string()}, g={self.__green.format_string()}, b={self.__blue.format_string()}, a={self.__alpha.format_string()})"


@final
class BitVecPrinter(gdb.ValuePrinter):
    def __init__(self, val: gdb.Value, storage_type_name: str):
        storage_type = gdb.lookup_type(storage_type_name)
        span = val["bitspan"]
        self.__length = span["len"]
        self.__ptr = span["ptr"]["pointer"].cast(storage_type.pointer())
        self.__storage_size = storage_type.sizeof

    def to_string(self):
        return f"BitVec(size={self.__length})"

    def children(self):
        for i in range(self.__length):
            bit_idx = i % self.__storage_size
            storage = self.__ptr[i // self.__storage_size]

            # Only `Lsb0` is supported.
            yield f"[{i}]", (storage >> bit_idx) & 1

    def num_children(self):
        return self.__length

    @staticmethod
    def display_hint():
        return "array"


def rgbds_lookup(val: gdb.Value):
    lookup_tag = val.type.tag
    if lookup_tag is None:
        return None

    if ARRAY_VEC_REGEX.match(lookup_tag):
        return ArrayVecPrinter(val)
    if lookup_tag == "compact_str::CompactString":
        return CompactStringPrinter(val)
    if ARIADNE_SOURCE_REGEX.match(lookup_tag):
        return AriadneSourcePrinter(val)
    if lookup_tag == "rgbgfx::color_set::ColorSet":
        return ColorGroupPrinter(val)
    if lookup_tag == "plumers::color::Rgb16":
        return Rgb16Printer(val)
    if lookup_tag == "bitvec::vec::BitVec<usize, bitvec::order::Lsb0>":
        return BitVecPrinter(val, "usize")

    return None


gdb.current_objfile().pretty_printers.append(rgbds_lookup)
