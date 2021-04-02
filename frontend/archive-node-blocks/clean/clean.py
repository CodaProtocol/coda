import json
import re
import sys

in_file_path = sys.argv[1]
out_file_path = sys.argv[2] 
value_regex = re.compile(r"\((\"?.*\"?)\)")
mongoose_type_regex = re.compile(r"\w+\((\"?.*\"?)\)")

out_file = open(out_file_path, 'w')

with open(in_file_path, 'r') as in_file:
    lines = in_file.readlines()
    prev_line = ""
    for i, line in enumerate(lines):
        type_value = mongoose_type_regex.search(line)
        value = value_regex.search(line)
        new_line = ""
        if (value is not None and type_value is not None):
            new_line = mongoose_type_regex.sub(value.group(1), line)

        if new_line == "":
            if (prev_line.strip() == "}" and line.strip() == '{'):
                lines[i-1] = lines[i-1].strip() + ",\n"

            lines[i] = line
        else:
            if (prev_line.strip() == "}" and line.strip() == '{'):
                lines[i-1] = lines[i-1].strip() + ",\n"

            lines[i] = new_line

        prev_line = line

    out_file.write('[')
    for line in lines:
        out_file.write(line)
    out_file.write(']')
