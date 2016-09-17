#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from collections import defaultdict
import json
import xlrd


def type_skeleton():
    """Structure for storing type information."""
    return {"base_type": None,
            "values": {"names": [], "codes": []}}


def empty_row(row):
    return not any(bool(cell) for cell in row)


def main(profile_fp):

    workbook = xlrd.open_workbook(profile_fp)
    types_sheet = workbook.sheet_by_name("Types")

    # Future proofing
    # ---------------
    # Some of the code below assumes the header
    # to be of the form...
    expected_header = ["type_name", "base_type",
                       "value_name", "value", "comment"]
    # So check against...
    actual_header = [cell.strip().replace(" ", "_").lower()
                     for cell in types_sheet.row_values(0)]

    assert all(expected in actual_header for expected in expected_header)

    types = defaultdict(type_skeleton)

    # i is our row counter; ignore the first (header) row.
    i, nrow = 1, types_sheet.nrows

    while i < nrow:
        # NB: there are various empty rows (sometimes more than one)
        # scattered throughout the Profile.xlsx file for sh*ts 'n' gigs.
        row = types_sheet.row_values(i)

        if empty_row(row):
            i += 1
            continue

        type_name = row[0]
        types[type_name]["base_type"] = row[1]

        # Because who doesn't love some feedback?
        print("Gathering type information for '%s'" % type_name)

        i += 1
        row = types_sheet.row_values(i)

        while not row[0]:  # Is the first column still empty?

            if empty_row(row):
                i += 1
                break

            types[type_name]["values"]["names"].append(row[2])

            # Some type codes are hexadecimals.
            try:
                code = int(row[3])
            except ValueError:
                code = int(row[3], 16)

            types[type_name]["values"]["codes"].append(code)

            i += 1
            if i == nrow:
                break
            else:
                row = types_sheet.row_values(i)

    return types


if __name__ == "__main__":
    to_dump = main("./Profile.xlsx")
    with open("./Types.json", "w") as handle:
        json.dump(to_dump, handle)
