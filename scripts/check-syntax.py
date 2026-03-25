#!/usr/bin/env python3
"""Check parenthesis/bracket/brace balancing in Clojure/ClojureScript files.

Catches unmatched delimiters that would cause compilation failures.
Handles string literals and comments (single-line ;;) to avoid false positives.
"""
import sys
import os
import glob

def check_file(filepath):
    """Check delimiter balancing in a single file. Returns list of error strings."""
    with open(filepath) as f:
        text = f.read()

    errors = []
    stack = []
    in_string = False
    in_comment = False
    escape_next = False
    skip_next = False

    for i, ch in enumerate(text):
        line = text[:i + 1].count('\n') + 1

        if skip_next:
            skip_next = False
            continue

        if escape_next:
            escape_next = False
            continue

        if ch == '\n':
            in_comment = False
            continue

        if in_comment:
            continue

        if ch == '\\' and in_string:
            escape_next = True
            continue

        # Character literals: \x, \[, \], \(, \), \{, \} etc.
        if ch == '\\' and not in_string:
            skip_next = True
            continue

        if ch == '"' and not in_comment:
            in_string = not in_string
            continue

        if in_string:
            continue

        if ch == ';':
            in_comment = True
            continue

        match = {'(': ')', '[': ']', '{': '}'}

        if ch in '([{':
            stack.append((ch, line))
        elif ch in ')]}':
            if not stack:
                errors.append(f"  line {line}: extra closing '{ch}'")
            else:
                open_ch, open_line = stack.pop()
                if match[open_ch] != ch:
                    errors.append(
                        f"  line {line}: '{ch}' closes '{open_ch}' from line {open_line}"
                    )

    for ch, line in stack:
        errors.append(f"  line {line}: unclosed '{ch}'")

    return errors


def main():
    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    patterns = ['src/**/*.clj', 'src/**/*.cljc', 'functions/src/**/*.cljs']
    files = []
    for pat in patterns:
        files.extend(glob.glob(os.path.join(root, pat), recursive=True))

    if not files:
        print("No Clojure/ClojureScript files found.")
        return 0

    failed = False
    for filepath in sorted(files):
        errors = check_file(filepath)
        rel = os.path.relpath(filepath, root)
        if errors:
            print(f"FAIL {rel}")
            for e in errors:
                print(e)
            failed = True
        else:
            print(f"  OK {rel}")

    if failed:
        print("\nSyntax check failed — unbalanced delimiters found.")
        return 1
    else:
        print(f"\nAll {len(files)} files OK.")
        return 0


if __name__ == '__main__':
    sys.exit(main())
