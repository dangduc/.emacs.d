"""Bounded structural evidence; never emit paths or transcript text."""
import collections
import json
import pathlib
import re

root = pathlib.Path.home() / ".codex/sessions"
paths = sorted(root.glob("*/*/*/rollout-*.jsonl"), key=lambda p: p.stat().st_mtime, reverse=True)[:100]
context = re.compile(r"^(?:# AGENTS\.md instructions|<(?:environment_context|INSTRUCTIONS|permissions|skills_instructions|user_instructions)(?:[ >]))")
first_categories = collections.Counter()
plugin_shapes = collections.Counter()
paired_first_lines = collections.Counter()
for path in paths:
    with path.open("rb") as stream:
        data = stream.read(262144)
    if path.stat().st_size > len(data):
        data = data.rsplit(b"\n", 1)[0]
    first = None
    event = None
    for line in data.splitlines():
        try:
            record = json.loads(line)
        except (ValueError, UnicodeError):
            continue
        payload = record.get("payload")
        if not isinstance(payload, dict):
            continue
        if record.get("type") == "event_msg" and payload.get("type") == "user_message" and event is None:
            if isinstance(payload.get("message"), str) and payload["message"].strip():
                event = payload["message"].strip()
        if record.get("type") != "response_item" or payload.get("type") != "message" or payload.get("role") != "user":
            continue
        for block in payload.get("content", []):
            if not isinstance(block, dict) or block.get("type") != "input_text" or not isinstance(block.get("text"), str):
                continue
            text = block["text"].strip()
            if text.startswith("<recommended_plugins>"):
                _, separator, suffix = text.partition("</recommended_plugins>")
                plugin_shapes["closed" if separator else "unclosed"] += 1
                plugin_shapes["empty_suffix" if not suffix.strip() else "nonempty_suffix"] += 1
            if text and not context.match(text) and first is None:
                first = text
    if first is not None:
        first_categories["recommended_plugins" if first.startswith("<recommended_plugins>") else "other"] += 1
    if first is not None and event is not None:
        paired_first_lines["equal" if first.splitlines()[0] == event.splitlines()[0] else "different"] += 1
print(json.dumps({"files": len(paths), "max_bytes_per_file": 262144, "first_fallback_categories": first_categories,
                  "plugin_block_shapes": plugin_shapes, "paired_first_lines": paired_first_lines}, indent=2))
