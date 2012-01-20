import sys
import os
import argparse
import shutil

def make_args_parser():
    parser = argparse.ArgumentParser(
            description="Break flatten LINC corpus file into a\
                         set of LINC files in the output directory,\
                         each sentence stored in a separated\
                         file.")
    parser.add_argument("input",
            help="LINC input file")
    parser.add_argument("output",
            help="Output directory")
    parser.add_argument("-w", "--overwrite",
            default=False, action="store_true",
            help="Overwrite files in the output directory.")
    return parser

def check_dir(path, overwrite):
    """Is string representing a valid, non-existing directory path ?"""
    if not os.path.isdir(os.path.dirname(path)):
        msg = "%s is not a valid path" % path
        raise argparse.ArgumentTypeError(msg)
    elif os.path.exists(path) and overwrite == False:
        msg = "%s already exists" % path
        raise argparse.ArgumentTypeError(msg)
    return path

def parse_args(parser):
    args = parser.parse_args()
    check_dir(args.output, args.overwrite)
    return args


def line_type(line, ltype):
    if line.strip().startswith(ltype):
        return True
    return False

def parse_word(word_str):
    orth = None
    nps = False
    disamb = None
    interps = set()
    for line in word_str:
        if line_type(line, "Orth"):
            orth = line.split()[1].strip()
        elif line_type(line, "Word"):
            if "Nps" in line:
                nps = True
        else:
            interp = line.split()[1].strip()
            if line.strip().startswith("*"):
                disamb = interp
            interps.add(interp)
    return Word(orth, nps, disamb, interps)

def parse(path):
    sent = []
    with open(path) as inp:
        for line in inp:
            if line.startswith("Sent") and sent != []:
                yield "".join(sent); sent = []
            sent.append(line)
        if sent != []:
            yield "".join(sent)

if __name__ == "__main__":
    parser = make_args_parser()
    args = parse_args(parser)

    if os.path.exists(args.output):
        shutil.rmtree(args.output)
    os.mkdir(args.output)

    for i, sent in enumerate(parse(args.input)):
        with open(os.path.join(args.output, str(i) + ".linc"), "w") as out:
            print >> out, sent
