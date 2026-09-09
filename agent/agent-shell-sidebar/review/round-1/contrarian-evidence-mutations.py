"""Create isolated mutations; the frozen source is never changed."""
from pathlib import Path

directory = Path(__file__).resolve().parent
source = (directory.parent / "baseline.el").read_text()
replacements = {
    "unbounded": ("(insert-file-contents file nil 0 8192)", "(insert-file-contents file)"),
    "separator-only": (
        r'"^\\(?:---[ \t]*$\\|## \\)"',
        r'"^---[ \t]*$"',
    ),
}
for name, (before, after) in replacements.items():
    assert source.count(before) == 1, (name, before, source.count(before))
    destination = directory / f"contrarian-evidence-{name}.el"
    destination.write_text(source.replace(before, after))
    print(f"{name}: {before} -> {after}; {destination.name}")
