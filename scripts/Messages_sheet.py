#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import json
import xlrd   # for development


def header_format(cell):
    cleaned = cell.strip().replace(" ", "_").lower()
    if "#" in cleaned:
        cleaned = "field_code"
    return cleaned


def empty_row(row):
    return not any([bool(cell) for cell in row])


def is_banner(row):
    # Based on banner text being placed in the 4th cell only.
    cells = [not bool(cell) for cell in row]
    fourth_cell = cells.pop(3)
    return not fourth_cell and all(cells)


def csv_to_array(str_):
    return str_.replace("\n", "").split(",")


def main(profile_fp):
    workbook = xlrd.open_workbook(profile_fp)
    mesg_sheet = workbook.sheet_by_name("Messages")

    header = mesg_sheet.row_values(0)

    header = [header_format(cell) for cell in header]
    # Don't need the first column, or anything after
    # (and including) the comment cell.
    header = header[1:header.index("comment")]

    messages = {}

    # i is our row counter; ignore the first (header) row.
    i, nrow = 1, mesg_sheet.nrows

    while i < nrow:
        # NB: there are various empty rows (sometimes more than one)
        # scattered throughout the Profile.xlsx file for sh*ts 'n' gigs.
        row = mesg_sheet.row_values(i)

        if empty_row(row) or is_banner(row):
            i += 1
            continue

        mesg_name = row[0]
        messages[mesg_name] = {}

        # Because who doesn't love some feedback?
        print("Gathering message information for '%s'" % mesg_name)

        i += 1
        row = mesg_sheet.row_values(i)

        in_subfield = False   # need to track this.
        previous_field = last_regular_field = None

        while not row[0]:  # Is the first column still empty?

            if empty_row(row) or is_banner(row):
                i += 1
                break

            # NB: len(header) < len(row)
            field = dict(zip(header, row[1:]))

            if field["field_code"] == "" and not in_subfield:
                in_subfield = True
                # There shouldn't be any need for this check, but still...
                if previous_field is not None:
                    previous_field["is_dynamic"] = True
                # Nor this one!
                if last_regular_field is not None:
                    field["dynamic_parent"] = last_regular_field

            elif in_subfield:  # reset
                in_subfield = False

            # Make ref_field_name/_value into arrays.
            if field["ref_field_name"]:
                field["ref_field_name"] = csv_to_array(field["ref_field_name"])

            if field["ref_field_value"]:
                field["ref_field_value"] = csv_to_array(field["ref_field_value"])

            # Need to index by field name, because
            # integer indexes will be lost to json
            # serialisation.
            field_name = field.pop("field_name")

            messages[mesg_name][field_name] = field

            i += 1
            if i == nrow:
                break
            else:
                previous_field = field
                row = mesg_sheet.row_values(i)

            if not in_subfield:
                last_regular_field = field_name

    return messages


if __name__ == "__main__":
    to_dump = main("./Profile.xlsx")
    with open("./Messages.json", "w") as handle:
        json.dump(to_dump, handle)
