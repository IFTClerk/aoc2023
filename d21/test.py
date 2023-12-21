#!/usr/bin/env python3

from copy import deepcopy
import copy
from math import ceil
import sys
from typing import List, Tuple

sys.setrecursionlimit(100000)
FILE = sys.argv[1] if len(sys.argv) > 1 else "input.txt"


def read_lines_to_list() -> (List[str], Tuple[int, int]):
    lines: List[str] = []
    line_count = 0
    with open(FILE, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            lines.append(list(line))
            if "S" in lines[-1]:
                start = (line_count, lines[-1].index("S"))
            line_count += 1

    return (lines, start)


def part_one():
    lines, start = read_lines_to_list()
    height = len(lines)
    width = len(lines[0])
    NUM_STEPS = 64 if FILE == "input.txt" else 6

    next_queue = [start]
    for _step in range(NUM_STEPS):
        curr_queue = deepcopy(next_queue)
        visited = set(deepcopy(next_queue))
        next_queue = []

        while curr_queue:
            curr = curr_queue.pop(0)

            for dir in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
                new_y, new_x = curr[0] + dir[0], curr[1] + dir[1]
                if (
                    new_y >= 0
                    and new_x >= 0
                    and new_y < height
                    and new_x < width
                    and (new_y, new_x) not in visited
                    and lines[new_y][new_x] != "#"
                ):
                    visited.add((new_y, new_x))
                    next_queue.append((new_y, new_x))

    print(f"Part 1: {len(next_queue)}")


def part_two():
    lines, start = read_lines_to_list()
    height = len(lines)
    width = len(lines[0])
    mod = 26501365 % height

    seen_states = []

    for run in [mod, mod + height, mod + height * 2]:
        next_queue = [start]
        for _ in range(run):
            curr_queue = deepcopy(next_queue)
            visited = set(deepcopy(next_queue))
            next_queue = []

            while curr_queue:
                curr = curr_queue.pop(0)

                for dir in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
                    new_y, new_x = curr[0] + dir[0], curr[1] + dir[1]

                    if (
                        lines[new_y % height][new_x % width] != "#"
                        and (new_y, new_x) not in visited
                    ):
                        visited.add((new_y, new_x))
                        next_queue.append((new_y, new_x))

        seen_states.append(len(next_queue))

    # seen_states = [3835, 34125, 94603] # hard-coded after running above since it's slow...
    print(seen_states)
    m = seen_states[1] - seen_states[0]
    n = seen_states[2] - seen_states[1]
    a = (n - m) // 2
    b = m - 3 * a
    c = seen_states[0] - b - a

    ceiling = ceil(26501365 / height)

    answer = a * ceiling**2 + b * ceiling + c

    print(f"Part 2: {answer}")


part_one()
part_two()
